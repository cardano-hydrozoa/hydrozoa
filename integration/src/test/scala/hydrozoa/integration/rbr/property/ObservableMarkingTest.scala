package hydrozoa.integration.rbr.property

import hydrozoa.integration.rbr.model.petri.hlpn.RBRHlNet
import hydrozoa.integration.rbr.model.petri.hlpn.RBRHlNet.BallotStatus.{Awaiting, Voted}
import hydrozoa.integration.rbr.model.petri.hlpn.RBRHlNet.RBRPlaceId.*
import org.scalacheck.{Prop, Properties}

object ObservableMarkingTest extends Properties("ObservableMarking"):

    private def net = RBRHlNet(nHeadPeers = 3, RbrSeed.committedObligations(2)).toOption.get

    // Deterministic example assertions checked once (no generators).
    private def allTrue(conds: Boolean*): Prop = Prop.all(conds.map(Prop.propBoolean)*)

    val _ = property(
      "alpha projects the RBR seed marking (public Voted box + 3 Awaiting peer boxes)"
    ) = {
        val obs = ObservableMarking.alpha(net)
        allTrue(
          obs.ballots == Map((Voted, BigInt(0)) -> 1, (Awaiting, BigInt(0)) -> 3),
          obs.counts == Map(
            UnresolvedTreasury -> 1,
            ResolvedTreasury -> 0,
            RegimeRef -> 1,
            DisputeScriptRef -> 1,
            TreasuryScriptRef -> 1,
            SetupLadder -> 7,
            EvacuationOutput -> 0,
            WithdrawalOutput -> 0
          )
        )
    }
