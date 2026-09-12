package hydrozoa.multisig.persistence.rocksdb

import cats.effect.{IO, Resource}
import cats.syntax.all.*
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.persistence.*
import hydrozoa.multisig.persistence.PersistenceEvent.{OpenRocksDbReady, OpenRocksDbStart}
import java.nio.file.{Files, Path}
import java.util.ArrayList as JArrayList
import org.rocksdb.{ColumnFamilyDescriptor, ColumnFamilyHandle, ColumnFamilyOptions, DBOptions, ReadOptions, RocksDB, WriteBatch as RWriteBatch, WriteOptions}
import scala.jdk.CollectionConverters.*

/** RocksDB-backed [[BackendStore]] implementation.
  *
  * Each [[Cf]] is opened as its own RocksDB column family; the default CF is opened too (RocksDB
  * requires it) but is not exposed. The on-disk CF name is the enum case name (UTF-8).
  *
  * Lifecycle is owned by [[RocksDbBackendStore.open]]: it returns a
  * `Resource[IO, BackendStore[IO]]` that loads the native library, opens the DB with all CFs, runs
  * the schema-version check, and cleans up handles + the DB on release.
  *
  * For the typed actor-facing API, wrap the returned `BackendStore` in
  * `Persistence.fromBackend(...)`.
  *
  * Native calls are wrapped in `IO.blocking` (RocksJava is synchronous and may block on disk I/O
  * and compaction).
  */
final class RocksDbBackendStore private (
    db: RocksDB,
    handles: Map[Cf, ColumnFamilyHandle],
    writeOptions: WriteOptions,
    readOptions: ReadOptions
) extends BackendStore[IO]:

    def get(cf: Cf, key: Array[Byte]): IO[Option[Array[Byte]]] =
        IO.blocking(Option(db.get(handles(cf), readOptions, key)))

    def put(cf: Cf, key: Array[Byte], value: Array[Byte]): IO[Unit] =
        IO.blocking(db.put(handles(cf), writeOptions, key, value))

    def delete(cf: Cf, key: Array[Byte]): IO[Unit] =
        IO.blocking(db.delete(handles(cf), writeOptions, key))

    def write(batch: RawWriteBatch): IO[Unit] =
        if batch.isEmpty then IO.unit
        else
            IO.blocking {
                val wb = new RWriteBatch()
                try
                    batch.ops.foreach {
                        case RawWriteBatch.Op.Put(cf, k, v) =>
                            wb.put(handles(cf), k, v)
                        case RawWriteBatch.Op.Delete(cf, k) =>
                            wb.delete(handles(cf), k)
                        case RawWriteBatch.Op.DeleteRange(cf, from, to) =>
                            wb.deleteRange(handles(cf), from, to)
                    }
                    db.write(writeOptions, wb)
                finally wb.close()
            }

    def cursor(
        cf: Cf,
        fromInclusive: Array[Byte]
    ): Resource[IO, BackendStore.Cursor[IO]] =
        Resource
            .fromAutoCloseable(IO.blocking {
                val it = db.newIterator(handles(cf), readOptions)
                it.seek(fromInclusive)
                it
            })
            .map(it =>
                new BackendStore.Cursor[IO]:
                    def next: IO[Option[(Array[Byte], Array[Byte])]] = IO.blocking {
                        if it.isValid then
                            val k = it.key()
                            val v = it.value()
                            it.next()
                            Some((k, v))
                        else None
                    }
            )

    def lastKey(cf: Cf): IO[Option[Array[Byte]]] =
        IO.blocking {
            val it = db.newIterator(handles(cf), readOptions)
            try
                it.seekToLast()
                if it.isValid then Some(it.key()) else None
            finally it.close()
        }

object RocksDbBackendStore:

    /** Open the RocksDB store at `path`, creating it (and parent directories) if it does not yet
      * exist. `cfs` is the config-derived column-family set to open (`Cf.mkAll(headPeers,
      * coilPeers, hubs)`, §7.1 — the per-author split makes the set membership-dependent). Runs the
      * schema-version check ([[StoreVersion]]) and refuses to open an incompatible store. Returns a
      * `Resource` that closes the DB and releases all native resources on use-completion.
      */
    def open(
        path: Path,
        cfs: List[Cf],
        identity: StoreIdentity,
        tracer: ContraTracer[IO, PersistenceEvent]
    ): Resource[IO, BackendStore[IO]] =
        openInternal(path, cfs, identity, tracer, readOnly = false)

    /** Open an existing store **read-only** — the mode `hydrozoa evacuate` uses (design
      * `docs/spec/evacuate-command.md`). The rule-based regime only reads persistence, so RO is
      * both correct and safer: it takes no exclusive lock and any stray write throws at the RocksDB
      * layer. Refuses a missing or uninitialized store (RO cannot create or version-stamp one).
      */
    def openReadOnly(
        path: Path,
        cfs: List[Cf],
        identity: StoreIdentity,
        tracer: ContraTracer[IO, PersistenceEvent]
    ): Resource[IO, BackendStore[IO]] =
        openInternal(path, cfs, identity, tracer, readOnly = true)

    private def openInternal(
        path: Path,
        cfs: List[Cf],
        identity: StoreIdentity,
        tracer: ContraTracer[IO, PersistenceEvent],
        readOnly: Boolean
    ): Resource[IO, BackendStore[IO]] =
        for
            _ <- Resource.eval(tracer.traceWith(OpenRocksDbStart(path)))
            _ <- Resource.eval(IO.blocking {
                RocksDB.loadLibrary()
                if readOnly then
                    if !Files.isDirectory(path) then
                        throw new IllegalStateException(
                          s"No RocksDB store to open read-only at $path"
                        )
                    else ()
                else
                    Files.createDirectories(path)
                    ()
            })
            cfOpts <- autoCloseable(new ColumnFamilyOptions())
            // Create flags are meaningless (and rejected) for a read-only open.
            dbOpts <- autoCloseable(
              if readOnly then hostBounds(new DBOptions())
              else
                  hostBounds(
                    new DBOptions()
                        .setCreateIfMissing(true)
                        .setCreateMissingColumnFamilies(true)
                  )
            )
            writeOptions <- autoCloseable(new WriteOptions())
            readOptions <- autoCloseable(new ReadOptions())
            opened <- openDb(path, dbOpts, cfOpts, cfs, readOnly)
            (db, handles) = opened
            backend = new RocksDbBackendStore(db, handles, writeOptions, readOptions)
            // Order matters: a store whose schema this build does not understand must not have
            // its metadata interpreted at all, and neither check may run after a recovery read.
            _ <- Resource.eval(versionCheck(backend, readOnly))
            _ <- Resource.eval(identityCheck(backend, identity, readOnly))
            _ <- Resource.eval(tracer.traceWith(OpenRocksDbReady(path, handles.size)))
        yield backend

    /** Two host-shaped bounds on this store's off-heap memory. Both default to RocksDB's own
      * values, so an environment that sets neither behaves exactly as it did before.
      *
      * `dbWriteBufferSize` caps total memtable memory for the whole DB, flushing the largest column
      * family when the cap is reached. It is the bound worth setting on a host whose memory is the
      * scarce resource, because the per-family sizes multiply and no single per-family setting
      * names the total: this store opens one column family per lane plus one per peer for each of
      * the four per-author families, so a two-peer head runs ~26 and a six-node fleet ~32. At the
      * stock 64 MB x 2 buffers each, that is gigabytes of headroom, and a host that runs out names
      * no memory setting as the cause. 0 leaves it uncapped, as today.
      *
      * `maxOpenFiles` bounds the table cache, which holds every open SST's index and filter blocks
      * and so grows with the file count rather than with the data. -1 keeps every file open, as
      * today; a bound trades an occasional reopen for a ceiling.
      */
    private def memoryBounds(opts: DBOptions): DBOptions =
        opts
            .setDbWriteBufferSize(envLong("HYDROZOA_ROCKSDB_DB_WRITE_BUFFER_BYTES", 0L))
            .setMaxOpenFiles(envInt("HYDROZOA_ROCKSDB_MAX_OPEN_FILES", -1))

    /** Every host-shaped bound this store reads from the environment. Split in two because the two
      * groups answer different questions — how much memory the store may hold, and how long it may
      * take to open — but they land on the same options object.
      */
    private def hostBounds(opts: DBOptions): DBOptions = openTimeBounds(memoryBounds(opts))

    /** Four bounds on how long `RocksDB.open` takes, three of which now default to something other
      * than RocksDB's own value.
      *
      * A node on the production box took **18 minutes** to open its store, and its `LOG` says where
      * the time went: the first WAL recovery starts 11 ms after the manifest read and the last one
      * finishes 1,097 s later. Table-handler preload — the one open cost that grows with the store
      * — was those 11 ms. Essentially all of the 18 minutes was **WAL replay**: 470 files, ~30 GB,
      * at an effective 27 MB/s on a volume that does 137.
      *
      * `maxTotalWalSize` is therefore the setting that matters, and 0 is a bad default because it
      * does not mean "unbounded" — it means *derived*, as `Σ_CF(writeBufferSize ×
      * maxWriteBufferNumber) × 4`. The measured store had 31 column families at the stock 64 MiB ×
      * 2, which derives a ceiling near 16 GB, and nothing in a slow start names a setting as its
      * cause.
      *
      * 2 GiB is chosen as roughly **one full write buffer per column family** (31 × 64 MiB ≈ 1.94
      * GiB). Below that the cap starts forcing partially-full memtables out on families that are
      * being written normally, trading replay time for write amplification; at or above it, the cap
      * only bites the pathology it is meant to catch — a rarely-written family pinning every WAL
      * segment it has a write in, which is every segment. At the replay rate measured above it puts
      * a **~80-second ceiling on WAL recovery** where there was an 18-minute floor.
      *
      * It bounds *future* growth only: a WAL already on disk is still replayed once, so this
      * shrinks the next open but not the one that adopts it.
      *
      * `skipStatsUpdateOnDbOpen` skips loading table properties from every file to seed compaction
      * statistics; the stats rebuild as compaction runs, so the cost is slightly worse compaction
      * decisions early on. `avoidUnnecessaryBlockingIo` moves obsolete-file deletion off the open
      * path onto a background job. Both are safe to have on by default and are why they now are.
      *
      * `skipCheckingSstFileSizesOnDbOpen` is deliberately left **off**. Unlike the other two it
      * does not skip bookkeeping, it skips verifying that each SST is the size the manifest claims
      * — which is how a truncated file is caught at open rather than at the read that needs it. Its
      * saving is a `stat` per file against a WAL replay that dominates the open by orders of
      * magnitude, so there is nothing to buy with it.
      */
    private def openTimeBounds(opts: DBOptions): DBOptions =
        opts
            .setMaxTotalWalSize(
              envLong("HYDROZOA_ROCKSDB_MAX_TOTAL_WAL_BYTES", 2L * 1024 * 1024 * 1024)
            )
            .setSkipStatsUpdateOnDbOpen(
              envBool("HYDROZOA_ROCKSDB_SKIP_STATS_UPDATE_ON_OPEN", true)
            )
            .setSkipCheckingSstFileSizesOnDbOpen(
              envBool("HYDROZOA_ROCKSDB_SKIP_SST_SIZE_CHECK_ON_OPEN", false)
            )
            .setAvoidUnnecessaryBlockingIO(envBool("HYDROZOA_ROCKSDB_AVOID_BLOCKING_IO", true))

    /** A numeric RocksDB knob read from the environment, falling back to the compiled-in default.
      * An unparseable value takes the default rather than failing the boot: these are host-shaped
      * tuning hints, not correctness settings, and a node that refuses to start is the worse
      * outcome.
      */
    private def envLong(name: String, default: Long): Long =
        sys.env.get(name).flatMap(_.trim.toLongOption).getOrElse(default)

    private def envInt(name: String, default: Int): Int =
        sys.env.get(name).flatMap(_.trim.toIntOption).getOrElse(default)

    private def envBool(name: String, default: Boolean): Boolean =
        sys.env.get(name).flatMap(_.trim.toBooleanOption).getOrElse(default)

    /** Run the open-time schema-version check. On a writable open a fresh store gets the current
      * version stamped; incompatible versions raise. A read-only open never writes, so a missing
      * version key is a hard error (an uninitialized store cannot be served read-only).
      */
    private def versionCheck(backend: BackendStore[IO], readOnly: Boolean): IO[Unit] =
        backend.get(Cf.Meta, StoreVersion.key).flatMap {
            case None =>
                if readOnly then
                    IO.raiseError(
                      new IllegalStateException(
                        s"Persistence store at $backend has no schema version " +
                            "(uninitialized); cannot open read-only"
                      )
                    )
                else
                    backend.put(
                      Cf.Meta,
                      StoreVersion.key,
                      StoreVersion.encode(StoreVersion.current)
                    )
            case Some(bytes) =>
                val found = StoreVersion.decode(bytes)
                if found == StoreVersion.current then IO.unit
                else
                    IO.raiseError(
                      new IllegalStateException(
                        s"Persistence schema version mismatch at $backend: " +
                            s"store reports $found, this build expects ${StoreVersion.current}"
                      )
                    )
        }

    /** Run the open-time identity check ([[StoreIdentity]]). On a writable open a fresh store gets
      * the current identity stamped; a mismatch raises, naming every field that differs.
      *
      * A read-only open never writes, so a missing stamp is a hard error there — it cannot stamp
      * one, and an unstamped store cannot be served. Same rule [[versionCheck]] follows.
      */
    private def identityCheck(
        backend: BackendStore[IO],
        identity: StoreIdentity,
        readOnly: Boolean
    ): IO[Unit] =
        for {
            stamped <- StoreIdentity.fields
                .traverse(f => backend.get(Cf.Meta, f.key).map(_.map(f.name -> _)))
                .map(_.flatten.toMap)
            _ <- StoreIdentity.check(stamped, identity) match {
                case StoreIdentity.Check.Fresh =>
                    if readOnly then
                        IO.raiseError(
                          new IllegalStateException(
                            s"Persistence store at $backend has no identity stamp " +
                                "(uninitialized); cannot open read-only"
                          )
                        )
                    else
                        StoreIdentity.fields.traverse_(f =>
                            backend.put(Cf.Meta, f.key, f.of(identity))
                        )
                case StoreIdentity.Check.Compatible => IO.unit
                case StoreIdentity.Check.Mismatch(problems) =>
                    IO.raiseError(
                      new IllegalStateException(
                        s"Persistence store at $backend belongs to a different head, " +
                            "configuration, or peer than this node: " +
                            problems.mkString("; ")
                      )
                    )
            }
        } yield ()

    private def openDb(
        path: Path,
        dbOpts: DBOptions,
        cfOpts: ColumnFamilyOptions,
        cfs: List[Cf],
        readOnly: Boolean
    ): Resource[IO, (RocksDB, Map[Cf, ColumnFamilyHandle])] =
        Resource
            .make(IO.blocking {
                // RocksDB requires the default CF to be opened too; we keep its handle in the
                // close-list but don't expose it through `Cf`.
                val descriptors = new JArrayList[ColumnFamilyDescriptor]()
                descriptors.add(new ColumnFamilyDescriptor(RocksDB.DEFAULT_COLUMN_FAMILY, cfOpts))
                cfs.foreach(cf =>
                    descriptors.add(new ColumnFamilyDescriptor(cfNameBytes(cf), cfOpts))
                )
                val outHandles = new JArrayList[ColumnFamilyHandle]()
                val db =
                    if readOnly then
                        RocksDB.openReadOnly(dbOpts, path.toString, descriptors, outHandles)
                    else RocksDB.open(dbOpts, path.toString, descriptors, outHandles)
                val allHandles = outHandles.asScala.toList
                // `allHandles.head` is the default CF; the tail aligns positionally with `cfs`.
                val handlesByCf: Map[Cf, ColumnFamilyHandle] = cfs.zip(allHandles.tail).toMap
                (db, allHandles, handlesByCf)
            }) { case (db, allHandles, _) =>
                IO.blocking {
                    allHandles.foreach(_.close())
                    db.close()
                }
            }
            .map { case (db, _, handlesByCf) => (db, handlesByCf) }

    /** Stable on-disk name for a CF — `Cf.name` in UTF-8 (per-author satellites embed the author).
      */
    private def cfNameBytes(cf: Cf): Array[Byte] = cf.name.getBytes("UTF-8")

    private def autoCloseable[A <: AutoCloseable](a: => A): Resource[IO, A] =
        Resource.fromAutoCloseable(IO.blocking(a))
