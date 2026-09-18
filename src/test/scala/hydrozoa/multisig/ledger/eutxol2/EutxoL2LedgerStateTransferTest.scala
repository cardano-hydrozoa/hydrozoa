package hydrozoa.multisig.ledger.eutxol2

import cats.data.NonEmptyList
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.eutxol2.store.InMemoryL2Store
import hydrozoa.multisig.ledger.eutxol2.tx.GenesisObligation
import hydrozoa.multisig.ledger.event.RequestId
import hydrozoa.multisig.ledger.l2.{Destination, L2CommandNumber, L2LedgerCommand, L2StateExport, RestoreError}
import org.scalacheck.Gen
import org.scalacheck.rng.Seed
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.{Coin, TransactionHash, TransactionInput, Value}
import scalus.uplc.builtin.Builtins.blake2b_256
import scalus.uplc.builtin.ByteString
import test.Generators.Hydrozoa.genGenesisObligation

/** Seeding one [[EutxoL2Ledger]] from another's exported state — the ledger half of a coil peer
  * joining at a start point (GUM-312).
  *
  * The property that matters is **the digests, not the bytes**: a joining peer checks what its own
  * ledger reports after adopting against an `l2StateHash` the head peers signed, so an export that
  * round-trips to a different `L2StateHash` is worthless however faithfully it encodes.
  */
class EutxoL2LedgerStateTransferTest extends AnyFunSuite:

    private val multiNodeConfig =
        MultiNodeConfig.generateDefault.pureApply(Gen.Parameters.default, Seed(0L))

    private val config: EutxoL2Ledger.Config = multiNodeConfig.nodeConfigs(HeadPeerNumber.zero)

    private val depositorAddress = multiNodeConfig.addressOf(HeadPeerNumber.zero)

    private def noop(n: Int): L2LedgerCommand.ApplyDepositDecisions =
        L2LedgerCommand.ApplyDepositDecisions(
          blockNumber = BlockNumber(n),
          blockCreationEndTime = BigInt(n),
          absorbedDeposits = Nil,
          rejectedDeposits = Nil
        )

    private val depositObligation = genGenesisObligation(
      depositorAddress,
      genValue = Gen.const(Value.ada(5))
    )(using config).pureApply(Gen.Parameters.default, Seed(1L))

    /** A real deposit, so the exported state carries a non-empty `pendingDeposits` compartment —
      * the one `L2Snapshot.stateHash` folds in third, and the one a purely no-op history would
      * leave empty and therefore untested.
      */
    private def registerDeposit(n: Int, requestId: RequestId): L2LedgerCommand.RegisterDeposit =
        L2LedgerCommand.RegisterDeposit(
          requestId = requestId,
          blockNumber = BlockNumber(n),
          blockCreationStartTime = BigInt(n),
          depositId = TransactionInput(
            TransactionHash.fromByteString(blake2b_256(ByteString.fromString(s"transfer-dep-$n"))),
            0
          ),
          depositFee = Coin.zero,
          depositL2Value = Value.ada(5),
          refundDestination = Destination(depositorAddress, None),
          l2Payload = GenesisObligation.serialize(NonEmptyList.one(depositObligation))
        )

    /** An `ApplyDepositDecisions` absorbing `requestId` — moves the deposit out of
      * `pendingDeposits` and into `activeUtxos`, so it is a command that actually changes state.
      */
    private def absorb(n: Int, requestId: RequestId): L2LedgerCommand.ApplyDepositDecisions =
        L2LedgerCommand.ApplyDepositDecisions(
          blockNumber = BlockNumber(n),
          blockCreationEndTime = BigInt(n),
          absorbedDeposits = List(requestId),
          rejectedDeposits = Nil
        )

    private val pendingRequest = RequestId(0, 7L)

    /** A donor whose state genuinely differs between command 3 and its tip at 5.
      *
      * ⚠️ The absorb at 4 is load-bearing. `L2Snapshot.stateHash` deliberately excludes
      * `commandNumber` — the same state digests the same whatever route reached it — so a history
      * of no-ops would leave every boundary digesting identically and a test comparing two of them
      * would pass without exercising anything.
      *
      *   - 1 registers a deposit (lands in `pendingDeposits`)
      *   - 2, 3 no-op — at 3 the deposit is still pending
      *   - 4 absorbs it (leaves `pendingDeposits`, enters `activeUtxos`)
      *   - 5 no-op — the tip
      */
    private def donorAt5: IO[EutxoL2Ledger] =
        for {
            store <- InMemoryL2Store.create
            ledger <- EutxoL2Ledger(config, store)
            _ <- ledger.registerDeposit(L2CommandNumber(1L), registerDeposit(1, pendingRequest))
            _ <- ledger.applyDepositDecisions(L2CommandNumber(2L), noop(2))
            _ <- ledger.applyDepositDecisions(L2CommandNumber(3L), noop(3))
            _ <- ledger.applyDepositDecisions(L2CommandNumber(4L), absorb(4, pendingRequest))
            _ <- ledger.applyDepositDecisions(L2CommandNumber(5L), noop(5))
        } yield ledger

    private val tip = L2CommandNumber(5L)

    private def freshLedger: IO[EutxoL2Ledger] =
        InMemoryL2Store.create.flatMap(EutxoL2Ledger(config, _))

    test("an exported state imports into a fresh ledger with identical digests") {
        val io = for {
            donorLedger <- donorAt5
            expected <- donorLedger.stateAt(tip).value
            exported <- donorLedger.exportStateAt(tip).value
            joiner <- freshLedger
            adopted <- joiner.importState(exported.toOption.get).value
        } yield assert(
          adopted == expected && adopted.isRight,
          s"adopted $adopted, donor reported $expected"
        )
        io.unsafeRunSync()
    }

    test("a state exported at a past boundary imports at that boundary, not at the donor's tip") {
        val past = L2CommandNumber(3L)
        val io = for {
            donorLedger <- donorAt5
            tipBefore <- donorLedger.stateAt(tip).value
            expected <- donorLedger.stateAt(past).value
            exported <- donorLedger.exportStateAt(past).value
            joiner <- freshLedger
            adopted <- joiner.importState(exported.toOption.get).value
            // Exporting a past boundary leaves the donor at its own tip.
            tipAfter <- donorLedger.stateAt(tip).value
        } yield {
            val _ =
                assert(adopted.isRight && adopted == expected, s"adopted $adopted, want $expected")
            val _ = assert(tipAfter == tipBefore, "exporting a past boundary moved the donor")
            assert(adopted != tipAfter, "the deposit absorbed at 4 should make 3 and 5 differ")
        }
        io.unsafeRunSync()
    }

    test("an adopted ledger can restoreTo its own start point afterwards") {
        val io = for {
            donorLedger <- donorAt5
            exported <- donorLedger.exportStateAt(tip).value
            joiner <- freshLedger
            adopted <- joiner.importState(exported.toOption.get).value
            // The snapshot landed in the joiner's store, so its own recovery path finds it — a
            // store whose tip is non-zero with no log behind it is exactly what seeding produces.
            restored <- joiner.restoreTo(tip).value
        } yield assert(
          restored == adopted && restored.isRight,
          s"restoreTo gave $restored, import gave $adopted"
        )
        io.unsafeRunSync()
    }

    test("importing into a ledger that has applied something is refused") {
        val io = for {
            donorLedger <- donorAt5
            exported <- donorLedger.exportStateAt(tip).value
            busy <- freshLedger
            _ <- busy.applyDepositDecisions(L2CommandNumber(1L), noop(1))
            adopted <- busy.importState(exported.toOption.get).value
        } yield assert(
          adopted.left.exists(_.isInstanceOf[RestoreError.StateImportRefused]),
          s"expected a StateImportRefused, got $adopted"
        )
        io.unsafeRunSync()
    }

    test("an export whose bytes do not decode is refused, not adopted") {
        val io = for {
            joiner <- freshLedger
            adopted <- joiner
                .importState(L2StateExport(L2CommandNumber(3L), IArray[Byte](1, 2, 3)))
                .value
        } yield assert(
          adopted.left.exists(_.isInstanceOf[RestoreError.StateImportRefused]),
          s"expected a StateImportRefused, got $adopted"
        )
        io.unsafeRunSync()
    }

    test("an export labelled with a command number it does not describe is refused") {
        val io = for {
            donorLedger <- donorAt5
            exported <- donorLedger.exportStateAt(tip).value
            mislabelled = exported.toOption.get.copy(commandNumber = L2CommandNumber(99L))
            joiner <- freshLedger
            adopted <- joiner.importState(mislabelled).value
        } yield assert(
          adopted.left.exists(_.isInstanceOf[RestoreError.StateImportRefused]),
          s"expected a StateImportRefused, got $adopted"
        )
        io.unsafeRunSync()
    }

    /** A transaction the ledger rejects: an unparseable L2 payload. It advances the command number
      * without being logged, which is the whole point.
      */
    private def rejectedTx(n: Int): L2LedgerCommand.ApplyTransaction =
        L2LedgerCommand.ApplyTransaction(
          requestId =
              RequestId(HeadPeerNumber.zero, hydrozoa.multisig.ledger.event.RequestNumber(n)),
          blockNumber = BlockNumber(n),
          blockCreationStartTime = BigInt(n),
          l2Payload = ByteString.fromString("not a transaction")
        )

    test("an export is labelled with the boundary it was asked for, not the last applied command") {
        // ⚠️ The gap every test above left open: they drive a history of **applied** commands, so
        // the reconstructed state's command number happens to match the boundary. A REJECTED
        // command advances the ledger without being logged, so reconstruction lands on the last
        // applied number instead — the state is right (a rejection changes nothing) but the label
        // is stale, and `importState` checks the label. A head whose L2 traffic is mostly invalid
        // transactions — which is what a quiet head looks like — could seed no one.
        val io = for {
            ledger <- freshLedger
            // One applied command, then rejections on top of it.
            _ <- ledger.applyDepositDecisions(L2CommandNumber(1L), noop(1))
            _ <- ledger.applyTransaction(L2CommandNumber(2L), rejectedTx(2))
            _ <- ledger.applyTransaction(L2CommandNumber(3L), rejectedTx(3))
            // Move the live position PAST the boundary we export, so the export goes through
            // reconstruction rather than reading the live state — the live state carries the right
            // number for free, which is what hid this.
            _ <- ledger.applyTransaction(L2CommandNumber(4L), rejectedTx(4))
            _ <- ledger.applyTransaction(L2CommandNumber(5L), rejectedTx(5))
            exported <- ledger.exportStateAt(L2CommandNumber(3L)).value
            joiner <- freshLedger
            adopted <- joiner.importState(exported.toOption.get).value
        } yield {
            val _ = assert(
              exported.toOption.get.commandNumber == L2CommandNumber(3L),
              "the export names the wrong boundary"
            )
            assert(adopted.isRight, s"an export over rejected commands would not import: $adopted")
        }
        io.unsafeRunSync()
    }
