package hydrozoa.multisig.ledger.remote

import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.event.RequestId
import hydrozoa.multisig.ledger.joint.{EvacuationDiff, EvacuationKey}
import hydrozoa.multisig.ledger.l2.L2LedgerResponse.DepositDecisionRejectReason
import hydrozoa.multisig.ledger.l2.{L2CommandNumber, L2LedgerResponse}
import io.circe.syntax.*
import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.ByteString

/** Round-trip tests for the `L2LedgerResponse` wire codec: every branch carries the command number,
  * and `Applied` has a concrete descendant per command (RegisterDeposit → none,
  * ApplyDepositDecisions → diffs, ApplyTransaction → diffs + payouts).
  */
class RemoteL2LedgerCodecsTest extends AnyFunSuite:

    private given CardanoNetwork.Section =
        MultiNodeConfig.generateDefault
            .map(_.nodeConfigs(HeadPeerNumber.zero))
            .pureApply(Gen.Parameters.default, org.scalacheck.rng.Seed(0L))

    import RemoteL2LedgerCodecs.given

    private def roundTrips(r: L2LedgerResponse): Boolean =
        io.circe.parser.decode[L2LedgerResponse](r.asJson.noSpaces) == Right(r)

    private val diff: EvacuationDiff =
        EvacuationDiff.Delete(EvacuationKey(ByteString.fromArray(Array.fill[Byte](36)(1))).get)

    test("Applied.RegisterDeposit (no effects) round-trips and carries no diff/payout fields") {
        val r = L2LedgerResponse.Applied.RegisterDeposit(L2CommandNumber(7L))
        val json = r.asJson.noSpaces
        assert(roundTrips(r) && !json.contains("evacuationDiffs") && !json.contains("payouts"))
    }

    test("Applied.ApplyDepositDecisions (diffs, no payouts) round-trips") {
        val r = L2LedgerResponse.Applied.ApplyDepositDecisions(L2CommandNumber(8L), Vector(diff))
        val json = r.asJson.noSpaces
        assert(roundTrips(r) && json.contains("evacuationDiffs") && !json.contains("payouts"))
    }

    test("Applied.ApplyTransaction (diffs + payouts) round-trips") {
        val r = L2LedgerResponse.Applied.ApplyTransaction(
          L2CommandNumber(9L),
          Vector(diff),
          Vector.empty
        )
        val json = r.asJson.noSpaces
        assert(roundTrips(r) && json.contains("evacuationDiffs") && json.contains("payouts"))
    }

    test("OutOfOrder carries the sent and the expected command number") {
        assert(roundTrips(L2LedgerResponse.OutOfOrder(L2CommandNumber(5L), L2CommandNumber(4L))))
    }

    test("LedgerFreeze carries the command number and the freezing decision's number") {
        assert(roundTrips(L2LedgerResponse.LedgerFreeze(L2CommandNumber(9L), L2CommandNumber(4L))))
    }

    test("Rejected.RegisterDeposit (string reason) round-trips") {
        assert(
          roundTrips(L2LedgerResponse.Rejected.RegisterDeposit(L2CommandNumber(2L), "bad deposit"))
        )
    }

    test("Rejected.ApplyTransaction (string reason) round-trips") {
        assert(
          roundTrips(L2LedgerResponse.Rejected.ApplyTransaction(L2CommandNumber(3L), "bad tx"))
        )
    }

    test("Rejected.ApplyDepositDecisions with CompartmentNotFound (typed reason) round-trips") {
        assert(
          roundTrips(
            L2LedgerResponse.Rejected.ApplyDepositDecisions(
              L2CommandNumber(4L),
              DepositDecisionRejectReason.CompartmentNotFound(RequestId(0, 9L))
            )
          )
        )
    }

    test("Rejected.ApplyDepositDecisions with InternalLedgerError (typed reason) round-trips") {
        assert(
          roundTrips(
            L2LedgerResponse.Rejected.ApplyDepositDecisions(
              L2CommandNumber(5L),
              DepositDecisionRejectReason.InternalLedgerError("merge failed")
            )
          )
        )
    }
