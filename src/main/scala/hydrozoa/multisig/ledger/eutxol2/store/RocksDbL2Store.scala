package hydrozoa.multisig.ledger.eutxol2.store

import cats.effect.{IO, Resource}
import hydrozoa.multisig.ledger.eutxol2.store.L2StoreCodecs.{realCommandCodec, snapshotCodec}
import hydrozoa.multisig.ledger.l2.{L2LedgerCommand, L2Serial}
import io.circe.syntax.*
import io.circe.{Decoder, Encoder}
import java.nio.ByteBuffer
import java.nio.file.{Files, Path}
import java.util.ArrayList as JArrayList
import org.rocksdb.{ColumnFamilyDescriptor, ColumnFamilyHandle, ColumnFamilyOptions, DBOptions, ReadOptions, RocksDB, WriteOptions}
import scala.jdk.CollectionConverters.*

/** RocksDB-backed [[L2Store]] — the durable form of the `EutxoL2Ledger` recovery store (§R2b).
  *
  * Two column families, both keyed by an 8-byte big-endian [[L2Serial]] (serials are non-negative,
  * so big-endian byte order coincides with numeric order, and RocksDB's lexicographic iteration is
  * serial order):
  *
  *   - **L2Log** — `serial -> ` JSON of the applied [[L2LedgerCommand.Real]].
  *   - **L2Snapshot** — `serial -> ` JSON of the [[L2Snapshot]].
  *
  * Values are the store-local JSON of [[L2StoreCodecs]] (UTF-8). This DB is wholly separate from
  * the consensus `BackendStore`: the L2 ledger owns its persistence, mirroring how a real black-box
  * L2 (SugarRush) would. Native RocksJava calls are wrapped in `IO.blocking`.
  */
final class RocksDbL2Store private (
    db: RocksDB,
    logCf: ColumnFamilyHandle,
    snapshotCf: ColumnFamilyHandle,
    writeOptions: WriteOptions,
    readOptions: ReadOptions
) extends L2Store[IO]:

    import RocksDbL2Store.{keyToSerial, serialToKey}

    def appendLog(serial: L2Serial, command: L2LedgerCommand.Real): IO[Unit] =
        IO.blocking(db.put(logCf, writeOptions, serialToKey(serial), encode(command)))

    def putSnapshot(serial: L2Serial, snapshot: L2Snapshot): IO[Unit] =
        IO.blocking(db.put(snapshotCf, writeOptions, serialToKey(serial), encode(snapshot)))

    def latestSnapshotAtOrBefore(serial: L2Serial): IO[Option[(L2Serial, L2Snapshot)]] =
        IO.blocking {
            val it = db.newIterator(snapshotCf, readOptions)
            try
                it.seekForPrev(serialToKey(serial))
                if it.isValid then Some(keyToSerial(it.key()) -> decode[L2Snapshot](it.value()))
                else None
            finally it.close()
        }

    def logRange(
        fromExclusive: L2Serial,
        toInclusive: L2Serial
    ): IO[List[L2LedgerCommand.Real]] =
        IO.blocking {
            val it = db.newIterator(logCf, readOptions)
            try
                // Seek to the first key >= fromExclusive, then drop the boundary key itself.
                it.seek(serialToKey(fromExclusive))
                val out = List.newBuilder[L2LedgerCommand.Real]
                var continue = true
                while continue && it.isValid do
                    val serial = keyToSerial(it.key())
                    if Ordering[L2Serial].gt(serial, toInclusive) then continue = false
                    else
                        if Ordering[L2Serial].gt(serial, fromExclusive) then
                            out += decode[L2LedgerCommand.Real](it.value())
                        it.next()
                out.result()
            finally it.close()
        }

    private def encode[A: Encoder](a: A): Array[Byte] = a.asJson.noSpaces.getBytes("UTF-8")

    private def decode[A: Decoder](bytes: Array[Byte]): A =
        io.circe.parser
            .decode[A](new String(bytes, "UTF-8"))
            .fold(
              err => throw new IllegalArgumentException(s"L2 store decode failed: $err"),
              identity
            )

object RocksDbL2Store:
    private val LogCfName = "L2Log".getBytes("UTF-8")
    private val SnapshotCfName = "L2Snapshot".getBytes("UTF-8")

    /** Open (creating if absent) the L2 store at `path`. The returned `Resource` closes the DB and
      * releases every native handle on completion.
      */
    def open(path: Path): Resource[IO, L2Store[IO]] =
        for
            _ <- Resource.eval(IO.blocking {
                RocksDB.loadLibrary()
                Files.createDirectories(path)
            })
            cfOpts <- autoCloseable(new ColumnFamilyOptions())
            dbOpts <- autoCloseable(
              new DBOptions().setCreateIfMissing(true).setCreateMissingColumnFamilies(true)
            )
            writeOptions <- autoCloseable(new WriteOptions())
            readOptions <- autoCloseable(new ReadOptions())
            opened <- openDb(path, dbOpts, cfOpts)
            (db, logCf, snapshotCf) = opened
        yield new RocksDbL2Store(db, logCf, snapshotCf, writeOptions, readOptions)

    private def openDb(
        path: Path,
        dbOpts: DBOptions,
        cfOpts: ColumnFamilyOptions
    ): Resource[IO, (RocksDB, ColumnFamilyHandle, ColumnFamilyHandle)] =
        Resource
            .make(IO.blocking {
                // RocksDB requires the default CF be opened; its handle is closed but never exposed.
                val descriptors = new JArrayList[ColumnFamilyDescriptor]()
                descriptors.add(new ColumnFamilyDescriptor(RocksDB.DEFAULT_COLUMN_FAMILY, cfOpts))
                descriptors.add(new ColumnFamilyDescriptor(LogCfName, cfOpts))
                descriptors.add(new ColumnFamilyDescriptor(SnapshotCfName, cfOpts))
                val outHandles = new JArrayList[ColumnFamilyHandle]()
                val db = RocksDB.open(dbOpts, path.toString, descriptors, outHandles)
                val handles = outHandles.asScala.toList
                (db, handles)
            }) { case (db, handles) =>
                IO.blocking {
                    handles.foreach(_.close())
                    db.close()
                }
            }
            // handles: [default, L2Log, L2Snapshot] — positional with the descriptor order above.
            .map { case (db, handles) => (db, handles(1), handles(2)) }

    /** 8-byte big-endian key; for non-negative serials this orders numerically. */
    private def serialToKey(serial: L2Serial): Array[Byte] =
        ByteBuffer.allocate(8).putLong(serial: Long).array()

    private def keyToSerial(key: Array[Byte]): L2Serial =
        L2Serial(ByteBuffer.wrap(key).getLong)

    private def autoCloseable[A <: AutoCloseable](a: => A): Resource[IO, A] =
        Resource.fromAutoCloseable(IO.blocking(a))
