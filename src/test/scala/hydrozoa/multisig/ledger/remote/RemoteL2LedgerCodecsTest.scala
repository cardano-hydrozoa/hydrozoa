package hydrozoa.multisig.ledger.remote

import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.joint.{EvacuationDiff, EvacuationKey}
import hydrozoa.multisig.ledger.l2.L2CommandNumber
import hydrozoa.multisig.ledger.remote.RemoteL2Ledger.Response
import io.circe.syntax.*
import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.ByteString

/** Round-trip tests for the `RemoteL2Ledger.Response` wire codec: every branch carries the command
  * number, and `Applied` omits empty effect fields on encode yet defaults them to empty on decode —
  * i.e. the per-command shape (RegisterDeposit → none, ApplyDepositDecisions → diffs,
  * ApplyTransaction → diffs + payouts).
  */
class RemoteL2LedgerCodecsTest extends AnyFunSuite:

    private given CardanoNetwork.Section =
        MultiNodeConfig.generateDefault
            .map(_.nodeConfigs(HeadPeerNumber.zero))
            .pureApply(Gen.Parameters.default, org.scalacheck.rng.Seed(0L))

    import RemoteL2LedgerCodecs.given

    private def roundTrips(r: Response): Boolean =
        io.circe.parser.decode[Response](r.asJson.noSpaces) == Right(r)

    private val diff: EvacuationDiff =
        EvacuationDiff.Delete(EvacuationKey(ByteString.fromArray(Array.fill[Byte](36)(1))).get)

    test(
      "Applied with no effects (RegisterDeposit shape) round-trips and omits the effect fields"
    ) {
        val r = Response.Applied(L2CommandNumber(7L), Vector.empty, Vector.empty)
        val json = r.asJson.noSpaces
        assert(roundTrips(r) && !json.contains("evacuationDiffs") && !json.contains("payouts"))
    }

    test("Applied with diffs but no payouts (ApplyDepositDecisions shape) round-trips") {
        val r = Response.Applied(L2CommandNumber(8L), Vector(diff), Vector.empty)
        val json = r.asJson.noSpaces
        assert(roundTrips(r) && json.contains("evacuationDiffs") && !json.contains("payouts"))
    }

    test("OutOfOrder carries the sent and the expected command number") {
        assert(roundTrips(Response.OutOfOrder(L2CommandNumber(5L), L2CommandNumber(4L))))
    }

    test("LedgerFreeze carries the command number and the freezing decision's number") {
        assert(roundTrips(Response.LedgerFreeze(L2CommandNumber(9L), L2CommandNumber(4L))))
    }

    test("Rejected carries the command number") {
        assert(roundTrips(Response.Rejected(L2CommandNumber(2L), "bad tx")))
    }
