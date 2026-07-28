package hydrozoa.integration.rbr.mbt

import hydrozoa.integration.rbr.model.petri.hlpn.RBRHlNet.BallotStatus.{Abstained, Voted}
import hydrozoa.integration.rbr.model.petri.hlpn.RBRHlNet.{BallotStatus, RBRPlaceId, RBRTransitionId}
import hydrozoa.lib.petri.hlpn.{Binding, HlNet, HlSimulator, ModeSelector}
import hydrozoa.multisig.consensus.peer.HeadPeerNumber

/** Drives the model `RBRHlNet` from its fallback seed to the all-evacuated terminal, mirroring the
  * head's autonomous dispute: three peers vote/abstain, the deadline passes, the boxes fold to the
  * single fully-tallied box, resolution reveals the resolved version, and evacuation drains its
  * whole committed batch. The terminal `ObservableMarking` is insensitive to the exact vote pattern
  * (which box/version won is projected away), so this fixed 3-peer sequence reaches the same
  * observable terminal as any autonomous run. Requires `nHeadPeers == 3` and
  * `maxVersionMinor >= 2`.
  */
object NetDriver:

    private type Net = HlNet[RBRPlaceId, RBRTransitionId, Any]

    private val (peer0, peer1, peer2) =
        (HeadPeerNumber(0), HeadPeerNumber(1), HeadPeerNumber(2))

    private def bindAll(n: Net, tid: RBRTransitionId)(values: Any*): Binding =
        n.transitionsMap(tid)
            .variables
            .zip(values)
            .foldLeft(Binding.empty) { case (acc, (v, value)) => Binding.bind(acc, v, value) }

    private def fired(n: Net, tid: RBRTransitionId, mode: Binding): Net =
        n.fire(tid, mode).toOption.get

    def driveToEvacuated(net: Net): Net =
        def vote(n: Net, p: HeadPeerNumber, k: Int, l: Int, v: Int): Net =
            fired(
              n,
              RBRTransitionId.Vote,
              bindAll(n, RBRTransitionId.Vote)(p, BigInt(k), BigInt(l), BigInt(v))
            )
        def tally(
            n: Net,
            tid: RBRTransitionId,
            q: HeadPeerNumber,
            cont: (Int, Int, BallotStatus, Int),
            rem: (Int, Int, BallotStatus, Int),
        ): Net =
            fired(
              n,
              tid,
              bindAll(n, tid)(
                q,
                BigInt(cont._1),
                BigInt(cont._2),
                cont._3,
                BigInt(cont._4),
                BigInt(rem._1),
                BigInt(rem._2),
                rem._3,
                BigInt(rem._4),
              )
            )

        var n = vote(net, peer0, 1, 2, 1)
        n = vote(n, peer1, 2, 3, 2)
        n = fired(
          n,
          RBRTransitionId.Abstain,
          bindAll(n, RBRTransitionId.Abstain)(peer2, BigInt(3), BigInt(0))
        )
        n = fired(n, RBRTransitionId.VotingDeadline, Binding.empty)
        n = tally(n, RBRTransitionId.TallyRemovedWins, peer0, (0, 1, Voted, 0), (1, 2, Voted, 1))
        n = tally(n, RBRTransitionId.TallyRemovedWins, peer0, (0, 2, Voted, 1), (2, 3, Voted, 2))
        n = tally(
          n,
          RBRTransitionId.TallyContinuingWins,
          peer0,
          (0, 3, Voted, 2),
          (3, 0, Abstained, 0)
        )
        n = fired(
          n,
          RBRTransitionId.Resolution,
          bindAll(n, RBRTransitionId.Resolution)(BigInt(2), peer0)
        )
        // Evacuation binds a Collect batch variable → drive via the unifying selector.
        HlSimulator(n, ModeSelector.unifying).fire(RBRTransitionId.Evacuation).toOption.get._1.net
