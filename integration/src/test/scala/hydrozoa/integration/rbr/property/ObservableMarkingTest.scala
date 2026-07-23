package hydrozoa.integration.rbr.property

import hydrozoa.integration.rbr.model.petri.hlpn.RBRHlNet
import hydrozoa.integration.rbr.model.petri.hlpn.RBRHlNet.BallotStatus.{Awaiting, Voted}
import hydrozoa.integration.rbr.model.petri.hlpn.RBRHlNet.RBRPlaceId.*
import org.scalatest.funsuite.AnyFunSuite

class ObservableMarkingTest extends AnyFunSuite:

    private def net = RBRHlNet(nHeadPeers = 3, maxVersionMinor = 2).toOption.get

    test("alpha projects the RBR seed marking (public Voted box + 3 Awaiting peer boxes)") {
        val obs = ObservableMarking.alpha(net)
        val _ = assert(
          obs.ballots == Map((Voted, BigInt(0)) -> 1, (Awaiting, BigInt(0)) -> 3)
        )
        assert(
          obs.counts == Map(
            UnresolvedTreasury -> 1,
            ResolvedTreasury   -> 0,
            RegimeRef          -> 1,
            DisputeScriptRef   -> 1,
            TreasuryScriptRef  -> 1,
            SetupLadder        -> 7,
            EvacuationOutput   -> 0
          )
        )
    }
