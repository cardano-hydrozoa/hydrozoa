package hydrozoa.multisig.ledger.eutxol2

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.eutxol2.store.{InMemoryL2Store, L2Store}
import hydrozoa.multisig.ledger.l2.{L2LedgerCommand, L2CommandNumber}
import org.scalacheck.Gen
import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite

/** R2b recovery tests for [[EutxoL2Ledger]] — commandNumber monotonicity, snapshot-interval, and
  * `restoreTo` reproducing the live state at any past commandNumber (snapshot boundary,
  * mid-interval, below the newest snapshot, and genesis).
  *
  * Uses the **no-op `ApplyDepositDecisions`** command (empty absorb / refund lists): it advances
  * the commandNumber and is logged like any real command, but does not touch `activeUtxos` and
  * never invokes the transaction mutator — so the test needs no constructed L2 tx / deposit
  * payloads, only a real config for the genesis utxos. Recovery correctness (commandNumber keying,
  * snapshot+log re-fold) is independent of which real command was applied.
  */
class EutxoL2LedgerRecoveryTest extends AnyFunSuite:

    /** A deterministic config sampled once from the default generator (fixed seed). */
    private val config: EutxoL2Ledger.Config =
        MultiNodeConfig.generateDefault
            .map(_.nodeConfigs(HeadPeerNumber.zero))
            .pureApply(Gen.Parameters.default, org.scalacheck.rng.Seed(0L))

    /** A no-op real command — bumps the commandNumber, logged, but leaves `activeUtxos` untouched.
      */
    private def noop(n: Int): L2LedgerCommand.ApplyDepositDecisions =
        L2LedgerCommand.ApplyDepositDecisions(
          blockNumber = BlockNumber(n),
          blockCreationEndTime = BigInt(n),
          absorbedDeposits = Nil,
          refundedDeposits = Nil
        )

    private final case class Run(
        ledger: EutxoL2Ledger,
        store: L2Store[IO],
        finalState: EutxoL2Ledger.State
    )

    /** Apply `count` no-op commands on a fresh ledger + store, returning both plus the final state.
      */
    private def runCommits(count: Int): IO[Run] =
        for
            store <- InMemoryL2Store.create
            ledger <- EutxoL2Ledger(config, store)
            _ <- (1 to count).toList.traverseVoid(i =>
                ledger.sendApplyDepositDecisions(noop(i)).value.flatMap(IO.fromEither)
            )
            finalState <- ledger.peekState
        yield Run(ledger, store, finalState)

    test("commandNumber advances by one per real command, starting from genesis 0") {
        run {
            for
                store <- InMemoryL2Store.create
                ledger <- EutxoL2Ledger(config, store)
                s0 <- ledger.currentCommandNumber
                _ <- ledger.sendApplyDepositDecisions(noop(1)).value.flatMap(IO.fromEither)
                s1 <- ledger.currentCommandNumber
                _ <- ledger.sendApplyDepositDecisions(noop(2)).value.flatMap(IO.fromEither)
                s2 <- ledger.currentCommandNumber
            yield assert(
              s0 == L2CommandNumber.zero && s1 == L2CommandNumber(1) && s2 == L2CommandNumber(2)
            )
        }
    }

    test(
      "restoreTo reproduces the live state at a snapshot boundary (commandNumber == SnapshotInterval)"
    ) {
        run {
            val n = L2Store.SnapshotInterval.toInt
            for
                run <- runCommits(n)
                // Fresh ledger over the same store, restored to the snapshot-boundary commandNumber.
                restored <- restoreFresh(run.store, L2CommandNumber(n.toLong))
            yield assert(
              restored.commandNumber == run.finalState.commandNumber
                  && restored.activeUtxos == run.finalState.activeUtxos
            )
        }
    }

    test("restoreTo reproduces the live state mid-interval (between snapshots)") {
        run {
            val target = L2Store.SnapshotInterval.toInt + 7 // past one snapshot, into the next run
            for
                run <- runCommits(target + 5) // commit beyond the target
                liveAtTarget <- replayLiveTo(target)
                restored <- restoreFresh(run.store, L2CommandNumber(target.toLong))
            yield assert(
              restored.commandNumber == L2CommandNumber(target.toLong)
                  && restored.activeUtxos == liveAtTarget.activeUtxos
            )
        }
    }

    test("restoreTo to a commandNumber below the newest snapshot still reconstructs correctly") {
        run {
            // Commit well past two snapshot boundaries, then restore to a target below the last one.
            val total = (L2Store.SnapshotInterval * 2).toInt + 3
            val target = L2Store.SnapshotInterval.toInt + 4 // < newest snapshot at 2*interval
            for
                run <- runCommits(total)
                liveAtTarget <- replayLiveTo(target)
                restored <- restoreFresh(run.store, L2CommandNumber(target.toLong))
            yield assert(
              restored.commandNumber == L2CommandNumber(target.toLong)
                  && restored.activeUtxos == liveAtTarget.activeUtxos
            )
        }
    }

    test("restoreTo genesis (commandNumber 0) yields the initial state") {
        run {
            for
                run <- runCommits(5)
                restored <- restoreFresh(run.store, L2CommandNumber.zero)
                genesis = EutxoL2Ledger.State.genesis(config)
            yield assert(
              restored.commandNumber == L2CommandNumber.zero && restored.activeUtxos == genesis.activeUtxos
            )
        }
    }

    test("restoreTo a commandNumber beyond the log fails rather than silently under-restoring") {
        run {
            for
                run <- runCommits(3)
                ledger <- EutxoL2Ledger(config, run.store)
                result <- ledger.restoreTo(L2CommandNumber(99)).value
            yield assert(result.isLeft)
        }
    }

    // --- helpers -------------------------------------------------------------

    /** A fresh ledger over `store`, restored to `commandNumber`; returns its post-restore state. */
    private def restoreFresh(
        store: L2Store[IO],
        commandNumber: L2CommandNumber
    ): IO[EutxoL2Ledger.State] =
        for
            ledger <- EutxoL2Ledger(config, store)
            _ <- ledger.restoreTo(commandNumber).value.flatMap(IO.fromEither)
            s <- ledger.peekState
        yield s

    /** The live state after replaying exactly `count` no-op commits on a fresh ledger. */
    private def replayLiveTo(count: Int): IO[EutxoL2Ledger.State] =
        runCommits(count).map(_.finalState)

    private def run(body: IO[Assertion]): Assertion = body.unsafeRunSync()
