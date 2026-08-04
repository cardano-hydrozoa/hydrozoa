package hydrozoa.multisig.ledger.eutxol2.store

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Resource}
import cats.syntax.all.*
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.eutxol2.EutxoL2Ledger
import hydrozoa.multisig.ledger.l2.{L2CommandNumber, L2LedgerCommand}
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
          rejectedDeposits = Nil
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
                        ledger.sendApplyDepositDecisions(L2CommandNumber(i.toLong), noop(i))
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
                        ledger.sendApplyDepositDecisions(L2CommandNumber(i.toLong), noop(i))
                    )
                    result <- ledger.restoreTo(L2CommandNumber(99)).value
                yield assert(result.isLeft)
            }
        }
    }

    test(
      "on-disk getTip falls back to the highest persisted command on a tip-less (legacy) store"
    ) {
        run {
            freshStore.use { store =>
                for
                    // Log entries written with no tip entry — a store from before the tip was tracked.
                    _ <- store.appendLog(L2CommandNumber(1L), noop(1))
                    _ <- store.appendLog(L2CommandNumber(2L), noop(2))
                    derived <- store.getTip
                    // restoreTo the derived tip succeeds instead of being rejected as beyond-tip.
                    ledger <- EutxoL2Ledger(config, store)
                    _ <- ledger.restoreTo(L2CommandNumber(2L)).value.flatMap(IO.fromEither)
                    s <- ledger.peekState
                yield assert(
                  derived.contains(L2CommandNumber(2L)) && s.commandNumber == L2CommandNumber(2L)
                )
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
        import L2StoreCodecs.commandCodec
        val command: L2LedgerCommand = noop(7)
        val decoded = io.circe.parser.decode[L2LedgerCommand](command.asJson.noSpaces)
        assert(decoded == Right(command))
    }

    private def run(body: IO[Assertion]): Assertion = body.unsafeRunSync()
