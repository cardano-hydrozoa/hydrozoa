package hydrozoa.multisig.ledger.eutxol2

import cats.data.NonEmptyList
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.eutxol2.store.{InMemoryL2Store, L2Snapshot, L2Store}
import hydrozoa.multisig.ledger.eutxol2.tx.GenesisObligation
import hydrozoa.multisig.ledger.event.RequestId
import hydrozoa.multisig.ledger.l2.{Destination, L2CommandNumber, L2LedgerCommand}
import org.scalacheck.Gen
import org.scalacheck.rng.Seed
import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.{Coin, TransactionHash, TransactionInput, Value}
import scalus.uplc.builtin.Builtins.blake2b_256
import scalus.uplc.builtin.ByteString
import test.Generators.Hydrozoa.genGenesisObligation

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

    /** A deterministic multi-node config + its derived single-node config, sampled once (fixed
      * seed).
      */
    private val multiNodeConfig =
        MultiNodeConfig.generateDefault.pureApply(Gen.Parameters.default, Seed(0L))

    private val config: EutxoL2Ledger.Config = multiNodeConfig.nodeConfigs(HeadPeerNumber.zero)

    private val depositorAddress = multiNodeConfig.addressOf(HeadPeerNumber.zero)

    /** A no-op real command — bumps the commandNumber, logged, but leaves `activeUtxos` untouched.
      */
    private def noop(n: Int): L2LedgerCommand.ApplyDepositDecisions =
        L2LedgerCommand.ApplyDepositDecisions(
          blockNumber = BlockNumber(n),
          blockCreationEndTime = BigInt(n),
          absorbedDeposits = Nil,
          rejectedDeposits = Nil
        )

    /** A command the ledger deterministically rejects: an `ApplyTransaction` whose `l2Payload` is
      * not a parseable L2 tx. It advances the coordination index but writes no log entry.
      */
    private val rejectingTx: L2LedgerCommand.ApplyTransaction =
        L2LedgerCommand.ApplyTransaction(
          requestId = RequestId(0, 0L),
          blockNumber = BlockNumber(1),
          blockCreationStartTime = BigInt(0),
          l2Payload = ByteString.fromArray(Array[Byte](0, 1, 2, 3))
        )

    /** A command that is impossible in correct operation: an `ApplyDepositDecisions` absorbing a
      * deposit that was never registered. The ledger fail-stops on it (an invariant violation), it
      * does not reject.
      */
    private def absorbingUnregistered(n: Int): L2LedgerCommand.ApplyDepositDecisions =
        L2LedgerCommand.ApplyDepositDecisions(
          blockNumber = BlockNumber(n),
          blockCreationEndTime = BigInt(n),
          absorbedDeposits = List(RequestId(0, 999L)),
          rejectedDeposits = Nil
        )

    /** A valid deposit whose single spawned output clears min-ada (5 ADA). */
    private val depositObligation = genGenesisObligation(
      depositorAddress,
      genValue = Gen.const(Value.ada(5))
    )(using config).pureApply(Gen.Parameters.default, Seed(1L))

    private val depositPayload = GenesisObligation.serialize(NonEmptyList.one(depositObligation))

    /** A valid `RegisterDeposit` for `requestId`, spawning [[depositObligation]]. */
    private def registerDeposit(n: Int, requestId: RequestId): L2LedgerCommand.RegisterDeposit =
        L2LedgerCommand.RegisterDeposit(
          requestId = requestId,
          blockNumber = BlockNumber(n),
          blockCreationStartTime = BigInt(n),
          depositId = TransactionInput(
            TransactionHash.fromByteString(blake2b_256(ByteString.fromString(s"recovery-dep-$n"))),
            0
          ),
          depositFee = Coin.zero,
          depositL2Value = Value.ada(5),
          refundDestination = Destination(depositorAddress, None),
          l2Payload = depositPayload
        )

    /** An `ApplyDepositDecisions` absorbing `requestId`. */
    private def absorb(n: Int, requestId: RequestId): L2LedgerCommand.ApplyDepositDecisions =
        L2LedgerCommand.ApplyDepositDecisions(
          blockNumber = BlockNumber(n),
          blockCreationEndTime = BigInt(n),
          absorbedDeposits = List(requestId),
          rejectedDeposits = Nil
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
                ledger
                    .sendApplyDepositDecisions(L2CommandNumber(i.toLong), noop(i))
            )
            finalState <- ledger.peekState
        yield Run(ledger, store, finalState)

    test("commandNumber advances by one per real command, starting from genesis 0") {
        run {
            for
                store <- InMemoryL2Store.create
                ledger <- EutxoL2Ledger(config, store)
                s0 <- ledger.peekState.map(_.commandNumber)
                _ <- ledger
                    .sendApplyDepositDecisions(L2CommandNumber(1L), noop(1))
                s1 <- ledger.peekState.map(_.commandNumber)
                _ <- ledger
                    .sendApplyDepositDecisions(L2CommandNumber(2L), noop(2))
                s2 <- ledger.peekState.map(_.commandNumber)
            yield assert(
              s0 == L2CommandNumber.zero && s1 == L2CommandNumber(1) && s2 == L2CommandNumber(2)
            )
        }
    }

    test(
      "ApplyDepositDecisions for an unregistered deposit fail-stops (invariant, not a verdict)"
    ) {
        run {
            for
                store <- InMemoryL2Store.create
                ledger <- EutxoL2Ledger(config, store)
                outcome <- ledger
                    .sendApplyDepositDecisions(L2CommandNumber(1L), absorbingUnregistered(1))
                    .attempt
                // The failed command advanced nothing: it raised before persisting the tip.
                tip <- ledger.peekState.map(_.commandNumber)
            yield assert(outcome.isLeft && tip == L2CommandNumber.zero)
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

    test(
      "a rejected command advances the coordination index without logging; restoreTo folds the gap"
    ) {
        run {
            for
                store <- InMemoryL2Store.create
                ledger <- EutxoL2Ledger(config, store)
                // 1: applied. 2: rejected (unparseable tx) — advances the tip, writes no log entry.
                // 3: applied — proving the index did not get stuck on the reject.
                _ <- ledger
                    .sendApplyDepositDecisions(L2CommandNumber(1L), noop(1))
                rejected <- ledger.sendApplyTransaction(L2CommandNumber(2L), rejectingTx).value
                tipAfterReject <- ledger.peekState.map(_.commandNumber)
                _ <- ledger
                    .sendApplyDepositDecisions(L2CommandNumber(3L), noop(3))
                tipAfter3 <- ledger.peekState.map(_.commandNumber)
                // restoreTo beyond the durable tip (4 > 3) is rejected — checked before the gap
                // restore below, which would regress the shared store's tip to 2.
                beyond <- EutxoL2Ledger(config, store)
                    .flatMap(_.restoreTo(L2CommandNumber(4L)).value)
                // restoreTo the gap index 2 reconstructs the state after command 1 (the reject
                // changed nothing) and adopts 2 as the tip.
                atGap <- restoreFresh(store, L2CommandNumber(2L))
            yield assert(
              rejected.isLeft
                  && tipAfterReject == L2CommandNumber(2L)
                  && tipAfter3 == L2CommandNumber(3L)
                  && atGap.commandNumber == L2CommandNumber(2L)
                  && beyond.isLeft
            )
        }
    }

    test(
      "restoreTo re-folds a real register->absorb deposit cycle (applyMutation on the replay path)"
    ) {
        run {
            val rid = RequestId(0, 1L)
            for
                store <- InMemoryL2Store.create
                ledger <- EutxoL2Ledger(config, store)
                _ <- ledger
                    .sendRegisterDeposit(L2CommandNumber(1L), registerDeposit(1, rid))
                    .value
                    .flatMap(IO.fromEither)
                _ <- ledger.sendApplyDepositDecisions(L2CommandNumber(2L), absorb(2, rid))
                live <- ledger.peekState
                genesis = EutxoL2Ledger.State.genesis(config)
                // Fresh ledger over the same store: restore re-folds the logged register + absorb
                // through applyMutation (base = genesis; no snapshot at <= 2).
                restored <- restoreFresh(store, L2CommandNumber(2L))
            yield assert(
              restored.commandNumber == L2CommandNumber(2L)
                  && restored.activeUtxos == live.activeUtxos
                  && restored.pendingDeposits == live.pendingDeposits
                  // the absorb actually added the deposit's spawned output, and cleared it from pending
                  && live.activeUtxos.size > genesis.activeUtxos.size
                  && live.pendingDeposits.isEmpty
            )
        }
    }

    test(
      "restoreTo through a snapshot carries a registered-but-unabsorbed deposit's pendingDeposits"
    ) {
        run {
            val rid = RequestId(0, 2L)
            val boundary = L2Store.SnapshotInterval.toInt
            for
                store <- InMemoryL2Store.create
                ledger <- EutxoL2Ledger(config, store)
                _ <- ledger
                    .sendRegisterDeposit(L2CommandNumber(1L), registerDeposit(1, rid))
                    .value
                    .flatMap(IO.fromEither)
                // Commit past the snapshot boundary so the pending deposit is baked into a snapshot.
                _ <- (2 to boundary + 1).toList.traverse_(i =>
                    ledger.sendApplyDepositDecisions(L2CommandNumber(i.toLong), noop(i))
                )
                live <- ledger.peekState
                restored <- restoreFresh(store, L2CommandNumber((boundary + 1).toLong))
            yield assert(
              restored.pendingDeposits == live.pendingDeposits
                  && restored.pendingDeposits.contains(rid)
                  && restored.activeUtxos == live.activeUtxos
            )
        }
    }

    test("getTip derives the tip from the highest persisted command on a tip-less (legacy) store") {
        run {
            val snap = L2Snapshot.fromState(EutxoL2Ledger.State.genesis(config))
            for
                store <- InMemoryL2Store.create
                _ <- store.appendLog(L2CommandNumber(1L), noop(1))
                _ <- store.appendLog(L2CommandNumber(4L), noop(4))
                _ <- store.putSnapshot(L2CommandNumber(6L), snap) // snapshot key above the log
                derived <- store.getTip
                fresh <- InMemoryL2Store.create.flatMap(_.getTip)
            yield assert(derived.contains(L2CommandNumber(6L)) && fresh.isEmpty)
        }
    }

    test("restoreTo on a tip-less (legacy) store uses the derived tip instead of rejecting") {
        run {
            for
                store <- InMemoryL2Store.create
                // A logged command with no tip entry — a store written before the tip was tracked.
                _ <- store.appendLog(L2CommandNumber(1L), noop(1))
                restored <- restoreFresh(store, L2CommandNumber(1L))
            yield assert(restored.commandNumber == L2CommandNumber(1L))
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
