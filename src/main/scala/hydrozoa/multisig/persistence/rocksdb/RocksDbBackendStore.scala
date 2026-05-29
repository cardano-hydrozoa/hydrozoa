package hydrozoa.multisig.persistence.rocksdb

import cats.effect.{IO, Resource}
import hydrozoa.lib.logging.Logging
import hydrozoa.multisig.persistence.*
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

    def lastKeyWithPrefix(cf: Cf, prefix: Array[Byte]): IO[Option[Array[Byte]]] =
        IO.blocking {
            // Upper bound for the prefix space: `prefix ++ 0xFF * 16`. Since our schema's keys
            // are ≤ 9 bytes (see §7.1), 16 trailing 0xFF bytes safely dominates every key
            // sharing this prefix; `seekForPrev` lands on the largest such key (or on a key
            // outside the prefix if none match, which the prefix check below rejects).
            val upperBound = prefix ++ Array.fill[Byte](16)(0xff.toByte)
            val it = db.newIterator(handles(cf), readOptions)
            try
                it.seekForPrev(upperBound)
                if it.isValid then
                    val k = it.key()
                    if RocksDbBackendStore.startsWith(k, prefix) then Some(k) else None
                else None
            finally it.close()
        }

object RocksDbBackendStore:
    private val logger = Logging.loggerIO("Persistence")

    /** Open the RocksDB store at `path`, creating it (and parent directories) if it does not yet
      * exist. Runs the schema-version check ([[StoreVersion]]) and refuses to open an incompatible
      * store. Returns a `Resource` that closes the DB and releases all native resources on
      * use-completion.
      */
    def open(path: Path): Resource[IO, BackendStore[IO]] =
        for
            _ <- Resource.eval(logger.info(s"opening RocksDB backend at $path"))
            _ <- Resource.eval(IO.blocking {
                RocksDB.loadLibrary()
                Files.createDirectories(path)
            })
            cfOpts <- autoCloseable(new ColumnFamilyOptions())
            dbOpts <- autoCloseable(
              new DBOptions()
                  .setCreateIfMissing(true)
                  .setCreateMissingColumnFamilies(true)
            )
            writeOptions <- autoCloseable(new WriteOptions())
            readOptions <- autoCloseable(new ReadOptions())
            opened <- openDb(path, dbOpts, cfOpts)
            (db, handles) = opened
            backend = new RocksDbBackendStore(db, handles, writeOptions, readOptions)
            _ <- Resource.eval(versionCheck(backend))
            _ <- Resource.eval(
              logger.info(s"RocksDB backend at $path ready (CFs=${handles.size})")
            )
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
        cfOpts: ColumnFamilyOptions
    ): Resource[IO, (RocksDB, Map[Cf, ColumnFamilyHandle])] =
        Resource
            .make(IO.blocking {
                // RocksDB requires the default CF to be opened too; we keep its handle in the
                // close-list but don't expose it through `Cf`.
                val descriptors = new JArrayList[ColumnFamilyDescriptor]()
                descriptors.add(new ColumnFamilyDescriptor(RocksDB.DEFAULT_COLUMN_FAMILY, cfOpts))
                Cf.all.foreach(cf =>
                    descriptors.add(new ColumnFamilyDescriptor(cfNameBytes(cf), cfOpts))
                )
                val outHandles = new JArrayList[ColumnFamilyHandle]()
                val db = RocksDB.open(dbOpts, path.toString, descriptors, outHandles)
                val allHandles = outHandles.asScala.toList
                // `allHandles.head` is the default CF; the tail aligns positionally with `Cf.all`.
                val handlesByCf: Map[Cf, ColumnFamilyHandle] = Cf.all.zip(allHandles.tail).toMap
                (db, allHandles, handlesByCf)
            }) { case (db, allHandles, _) =>
                IO.blocking {
                    allHandles.foreach(_.close())
                    db.close()
                }
            }
            .map { case (db, _, handlesByCf) => (db, handlesByCf) }

    /** Stable on-disk name for a CF — the enum case name in UTF-8. */
    private def cfNameBytes(cf: Cf): Array[Byte] = cf.toString.getBytes("UTF-8")

    private def autoCloseable[A <: AutoCloseable](a: => A): Resource[IO, A] =
        Resource.fromAutoCloseable(IO.blocking(a))

    /** True iff `bytes` is at least as long as `prefix` and matches it byte-for-byte. */
    private def startsWith(bytes: Array[Byte], prefix: Array[Byte]): Boolean =
        if bytes.length < prefix.length then false
        else
            var i = 0
            var ok = true
            while ok && i < prefix.length do
                if bytes(i) != prefix(i) then ok = false
                i += 1
            ok
