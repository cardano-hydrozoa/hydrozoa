package hydrozoa.multisig.ledger.eutxol2.store

import cats.effect.{IO, Ref, Resource}
import hydrozoa.multisig.ledger.l2.{L2CommandNumber, L2LedgerCommand}
import scala.collection.immutable.TreeMap

/** In-memory [[L2Store]] for tests. Holds typed values directly — no serialization — so it
  * exercises all the recovery *logic* (commandNumber keying, snapshot-interval,
  * restore-from-snapshot, log re-fold) without the codec / RocksDB plumbing. Not durable. The
  * RocksDB impl mirrors this contract on disk.
  */
object InMemoryL2Store:

    /** Build a fresh in-memory store. */
    def create: IO[L2Store[IO]] =
        for
            log <- Ref.of[IO, TreeMap[L2CommandNumber, L2LedgerCommand.Real]](TreeMap.empty)
            snapshots <- Ref.of[IO, TreeMap[L2CommandNumber, L2Snapshot]](TreeMap.empty)
        yield new Impl(log, snapshots)

    /** Resource form, for parity with the RocksDB factory; the release is a no-op. */
    def open: Resource[IO, L2Store[IO]] = Resource.eval(create)

    private final class Impl(
        log: Ref[IO, TreeMap[L2CommandNumber, L2LedgerCommand.Real]],
        snapshots: Ref[IO, TreeMap[L2CommandNumber, L2Snapshot]]
    ) extends L2Store[IO]:

        def appendLog(commandNumber: L2CommandNumber, command: L2LedgerCommand.Real): IO[Unit] =
            log.update(_.updated(commandNumber, command))

        def putSnapshot(commandNumber: L2CommandNumber, snapshot: L2Snapshot): IO[Unit] =
            snapshots.update(_.updated(commandNumber, snapshot))

        def latestSnapshotAtOrBefore(
            commandNumber: L2CommandNumber
        ): IO[Option[(L2CommandNumber, L2Snapshot)]] =
            snapshots.get.map(_.rangeTo(commandNumber).lastOption)

        def logRange(
            fromExclusive: L2CommandNumber,
            toInclusive: L2CommandNumber
        ): IO[List[L2LedgerCommand.Real]] =
            log.get.map(
              _.rangeFrom(fromExclusive).rangeTo(toInclusive).removed(fromExclusive).values.toList
            )
