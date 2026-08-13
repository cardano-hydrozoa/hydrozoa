package hydrozoa.multisig.persistence.rocksdb

import cats.effect.{IO, Resource}
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.persistence.*
import hydrozoa.multisig.persistence.PersistenceEvent.{OpenRocksDbReady, OpenRocksDbStart}
import java.nio.file.{Files, Path}
import java.util.ArrayList as JArrayList
import org.rocksdb.{ColumnFamilyDescriptor, ColumnFamilyHandle, ColumnFamilyOptions, DBOptions, ReadOptions, RocksDB, Statistics, WriteBatch as RWriteBatch, WriteOptions}
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
        tracer: ContraTracer[IO, PersistenceEvent]
    ): Resource[IO, BackendStore[IO]] =
        for
            _ <- Resource.eval(tracer.traceWith(OpenRocksDbStart(path)))
            _ <- Resource.eval(IO.blocking {
                RocksDB.loadLibrary()
                Files.createDirectories(path)
            })
            cfOpts <- autoCloseable(tunedColumnFamilyOptions())
            stats <- autoCloseable(new Statistics())
            dbOpts <- autoCloseable(tunedDbOptions(stats))
            writeOptions <- autoCloseable(new WriteOptions())
            readOptions <- autoCloseable(new ReadOptions())
            opened <- openDb(path, dbOpts, cfOpts, cfs)
            (db, handles) = opened
            backend = new RocksDbBackendStore(db, handles, writeOptions, readOptions)
            _ <- Resource.eval(versionCheck(backend))
            _ <- Resource.eval(tracer.traceWith(OpenRocksDbReady(path, handles.size)))
        yield backend

    /** Run the open-time schema-version check. Fresh stores get the current version stamped;
      * incompatible versions raise.
      */
    private def versionCheck(backend: BackendStore[IO]): IO[Unit] =
        backend.get(Cf.Meta, StoreVersion.key).flatMap {
            case None =>
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

    private def openDb(
        path: Path,
        dbOpts: DBOptions,
        cfOpts: ColumnFamilyOptions,
        cfs: List[Cf]
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
                val db = RocksDB.open(dbOpts, path.toString, descriptors, outHandles)
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

    /** Per-column-family options tuned to absorb write bursts without stalling.
      *
      * All CFs (admission's Request lane plus every settlement lane) share this one options object,
      * and they share a single write path. Under a settlement burst the stock defaults stall the
      * whole DB — either on a full memtable (`maxWriteBufferNumber = 2`) or on the L0-file count
      * (`level0StopWritesTrigger = 36`) — which blocks the RequestSequencer's per-request persist and
      * shows up as multi-second admission latency. We have large disk headroom, so we widen both
      * limits: more (and larger) memtables to soak up bursts while flushes drain, and much higher L0
      * slowdown/stop triggers so compaction backlog throttles writes far later. Every value is
      * overridable by env for no-recompile A/B tuning; the defaults are the tuned values (set the env
      * vars back to 67108864 / 2 / 4 / 20 / 36 to reproduce stock behaviour).
      */
    private def tunedColumnFamilyOptions(): ColumnFamilyOptions =
        new ColumnFamilyOptions()
            .setWriteBufferSize(envLong("HYDROZOA_ROCKSDB_WRITE_BUFFER_BYTES", 128L * 1024 * 1024))
            .setMaxWriteBufferNumber(envInt("HYDROZOA_ROCKSDB_MAX_WRITE_BUFFERS", 6))
            .setMinWriteBufferNumberToMerge(envInt("HYDROZOA_ROCKSDB_MIN_MERGE", 1))
            .setLevel0FileNumCompactionTrigger(envInt("HYDROZOA_ROCKSDB_L0_COMPACTION", 8))
            .setLevel0SlowdownWritesTrigger(envInt("HYDROZOA_ROCKSDB_L0_SLOWDOWN", 40))
            .setLevel0StopWritesTrigger(envInt("HYDROZOA_ROCKSDB_L0_STOP", 80))

    /** Whole-DB options: size the background flush/compaction pool so it keeps up with the widened
      * memtable/L0 budget above (a 32-thread box; stock `maxBackgroundJobs` is 2), spread large
      * compactions across subcompactions, and range-sync in ~1 MB chunks so the OS writes dirty pages
      * back incrementally instead of dumping a large buffer at once (smooths the latency tail even
      * with `sync=false`). `stats` + a 30 s stats-dump make the DB's `LOG` record cumulative
      * stall-micros and per-level compaction so we can confirm the stalls are actually gone.
      */
    private def tunedDbOptions(stats: Statistics): DBOptions =
        new DBOptions()
            .setCreateIfMissing(true)
            .setCreateMissingColumnFamilies(true)
            .setIncreaseParallelism(envInt("HYDROZOA_ROCKSDB_BG_JOBS", 8))
            .setMaxBackgroundJobs(envInt("HYDROZOA_ROCKSDB_BG_JOBS", 8))
            .setMaxSubcompactions(envInt("HYDROZOA_ROCKSDB_SUBCOMPACTIONS", 4))
            .setBytesPerSync(envLong("HYDROZOA_ROCKSDB_BYTES_PER_SYNC", 1L * 1024 * 1024))
            .setWalBytesPerSync(envLong("HYDROZOA_ROCKSDB_WAL_BYTES_PER_SYNC", 1L * 1024 * 1024))
            .setStatistics(stats)
            .setStatsDumpPeriodSec(envInt("HYDROZOA_ROCKSDB_STATS_DUMP_SEC", 30))

    private def envInt(name: String, default: Int): Int =
        sys.env.get(name).flatMap(_.toIntOption).getOrElse(default)

    private def envLong(name: String, default: Long): Long =
        sys.env.get(name).flatMap(_.toLongOption).getOrElse(default)

    private def autoCloseable[A <: AutoCloseable](a: => A): Resource[IO, A] =
        Resource.fromAutoCloseable(IO.blocking(a))
