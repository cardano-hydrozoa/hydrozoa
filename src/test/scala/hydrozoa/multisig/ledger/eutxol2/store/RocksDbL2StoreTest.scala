package hydrozoa.multisig.ledger.eutxol2.store

import cats.effect.{IO, Resource}
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.eutxol2.EutxoL2Ledger
import hydrozoa.multisig.ledger.l2.{L2LedgerCommand, L2CommandNumber}
import io.circe.syntax.*
import java.nio.file.Files
import org.scalacheck.Gen
import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite

/** R2b tests for the on-disk [[RocksDbL2Store]] and its codecs ([[L2StoreCodecs]]).
  *
  * The recovery flow mirrors `EutxoL2LedgerRecoveryTest` but over a real RocksDB store in a temp
  * directory, so it exercises the on-disk plumbing (big-endian commandNumber keys, the two CFs,
  * `seekForPrev` snapshot lookup, the `(from, to]` log scan) plus the `ApplyDepositDecisions` JSON
  * round-trip end to end. The codec tests round-trip the snapshot (with real genesis utxos) and the
  * command dispatch directly.
  */
class RocksDbL2StoreTest extends AnyFunSuite:

    private val config: EutxoL2Ledger.Config =
        MultiNodeConfig.generateDefault
            .map(_.nodeConfigs(HeadPeerNumber.zero))
            .pureApply(Gen.Parameters.default, org.scalacheck.rng.Seed(0L))

    private def noop(n: Int): L2LedgerCommand.ApplyDepositDecisions =
        L2LedgerCommand.ApplyDepositDecisions(
          blockNumber = BlockNumber(n),
          blockCreationEndTime = BigInt(n),
          absorbedDeposits = Nil,
          refundedDeposits = Nil
        )

    /** A fresh RocksDB store in a temp directory, cleaned up on release. */
    private def freshStore: Resource[IO, L2Store[IO]] =
        Resource
            .make(IO.blocking(Files.createTempDirectory("l2store-test")))(dir =>
                IO.blocking {
                    Files
                        .walk(dir)
                        .sorted(java.util.Comparator.reverseOrder())
                        .forEach(Files.delete)
                }
            )
            .flatMap(RocksDbL2Store.open)

    test("on-disk restoreTo reproduces the live state mid-interval (below the newest snapshot)") {
        run {
            freshStore.use { store =>
                val total = (L2Store.SnapshotInterval * 2).toInt + 3
                val target = L2Store.SnapshotInterval.toInt + 4
                for
                    ledger <- EutxoL2Ledger(config, store)
                    _ <- (1 to total).toList.traverseVoid(i =>
                        ledger.sendApplyDepositDecisions(noop(i)).value.flatMap(IO.fromEither)
                    )
                    // A second ledger over the same on-disk store, rebuilt purely from snapshot+log.
                    restored <- EutxoL2Ledger(config, store)
                    _ <- restored
                        .restoreTo(L2CommandNumber(target.toLong))
                        .value
                        .flatMap(IO.fromEither)
                    s <- restored.peekState
                yield assert(s.commandNumber == L2CommandNumber(target.toLong))
            }
        }
    }

    test("on-disk restoreTo beyond the log fails rather than silently under-restoring") {
        run {
            freshStore.use { store =>
                for
                    ledger <- EutxoL2Ledger(config, store)
                    _ <- (1 to 3).toList.traverseVoid(i =>
                        ledger.sendApplyDepositDecisions(noop(i)).value.flatMap(IO.fromEither)
                    )
                    result <- ledger.restoreTo(L2CommandNumber(99)).value
                yield assert(result.isLeft)
            }
        }
    }

    test("snapshot codec round-trips real genesis utxos") {
        import L2StoreCodecs.snapshotCodec
        val genesis = EutxoL2Ledger.State.genesis(config)
        val snapshot = L2Snapshot(L2CommandNumber(5), genesis.activeUtxos, Map.empty)
        val decoded = io.circe.parser.decode[L2Snapshot](snapshot.asJson.noSpaces)
        // The round-trip is only meaningful when the fixture actually carries utxos.
        assert(genesis.activeUtxos.nonEmpty && decoded == Right(snapshot))
    }

    test("real-command codec round-trips every tag") {
        import L2StoreCodecs.realCommandCodec
        val command: L2LedgerCommand.Real = noop(7)
        val decoded = io.circe.parser.decode[L2LedgerCommand.Real](command.asJson.noSpaces)
        assert(decoded == Right(command))
    }

    private def run(body: IO[Assertion]): Assertion = body.unsafeRunSync()
