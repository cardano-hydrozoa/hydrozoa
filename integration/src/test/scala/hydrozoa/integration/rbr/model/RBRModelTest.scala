package hydrozoa.integration.rbr.model

import hydrozoa.integration.rbr.model.petri.net.{RBRNet, RBRPlaceId}
import hydrozoa.integration.rbr.model.petri.net.RBRPlaceId.*
import org.scalatest.funsuite.AnyFunSuite

/** Unit tests for the [[RBRModel]] driver: given a set of `RBRNet.Params`, the driver's terminal
  * marking must match the analytical expected map.
  *
  * The expected terminal is derived from three invariants:
  *   - All ballot places (Unvoted / Voted / Abstain) drain to 0 via Vote/Tally/Resolution.
  *   - `UnresolvedTreasuryPlaceId` drains to 0, `ResolvedTreasuryPlaceId` reaches 1.
  *   - `PayoutObligationsPlaceId` drains to 0 while `EvacuationOutputPlaceId` grows to `nEvacs`.
  *   - Ref-type places (TreasuryRef, DisputeRef, RegimeRef, SetupLadderRef) are unchanged.
  *   - `AmbientPlaceId` drains to 0 (seeded at exactly `2 * nEvacFirings`).
  *   - `CollateralPlaceId` is unchanged (only read, never spent, at marking level).
  */
class RBRModelTest extends AnyFunSuite {

    private def expected(params: RBRNet.Params): Map[RBRPlaceId, Int] = Map(
      TreasuryRefPlaceId -> 1,
      DisputeRefPlaceId -> 1,
      RegimeRefPlaceId -> 1,
      SetupLadderRefPlaceId -> 7,
      UnresolvedTreasuryPlaceId -> 0,
      ResolvedTreasuryPlaceId -> 1,
      UnvotedPlaceId -> 0,
      VotedPlaceId -> 0,
      AbstainPlaceId -> 0,
      TallyBufferUnvotedPlaceId -> 0,
      TallyBufferAbstainPlaceId -> 0,
      TallyBufferVotedPlaceId -> 0,
      PayoutObligationsPlaceId -> 0,
      EvacuationOutputPlaceId -> params.nEvacs,
      CollateralPlaceId -> params.nHeadPeers,
      AmbientPlaceId -> 0,
    )

    private val scenarios: List[RBRNet.Params] = List(
      RBRNet.Params(nHeadPeers = 1, nEvacs = 0, payoutBatchSize = 1),
      RBRNet.Params(nHeadPeers = 1, nEvacs = 1, payoutBatchSize = 1),
      RBRNet.Params(nHeadPeers = 3, nEvacs = 0, payoutBatchSize = 63),
      RBRNet.Params(nHeadPeers = 3, nEvacs = 63, payoutBatchSize = 63),
      RBRNet.Params(nHeadPeers = 3, nEvacs = 126, payoutBatchSize = 63),
      RBRNet.Params(nHeadPeers = 3, nEvacs = 100, payoutBatchSize = 10),
      RBRNet.Params(nHeadPeers = 7, nEvacs = 21, payoutBatchSize = 3),
      RBRNet.Params(nHeadPeers = 20, nEvacs = 1_000, payoutBatchSize = 100),
    )

    scenarios.foreach { params =>
        test(s"terminal marking is analytical for $params") {
            val actual = RBRModel.terminal(params)
            assert(
              actual == expected(params),
              s"\nexpected: ${expected(params)}\nactual:   $actual"
            )
        }
    }

    test("Params.require rejects nEvacs not divisible by payoutBatchSize") {
        intercept[IllegalArgumentException] {
            RBRNet.Params(nHeadPeers = 3, nEvacs = 100, payoutBatchSize = 63)
        }
    }

    test("Params.require rejects nHeadPeers < 1") {
        intercept[IllegalArgumentException] {
            RBRNet.Params(nHeadPeers = 0, nEvacs = 0, payoutBatchSize = 1)
        }
    }

    test("Params.require rejects payoutBatchSize < 1") {
        intercept[IllegalArgumentException] {
            RBRNet.Params(nHeadPeers = 1, nEvacs = 0, payoutBatchSize = 0)
        }
    }

    test("RBRNet topology is valid (no dangling arcs, no duplicate (place, transition) pairs)") {
        scenarios.foreach { params =>
            val net = RBRNet(params)
            val errors = net.topologyErrors
            assert(errors.isEmpty, s"Topology errors for $params: $errors")
        }
    }
}
