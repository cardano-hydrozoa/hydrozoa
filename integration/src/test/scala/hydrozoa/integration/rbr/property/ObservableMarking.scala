package hydrozoa.integration.rbr.property

import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.integration.rbr.model.petri.hlpn.RBRHlNet.Ballot.*
import hydrozoa.integration.rbr.model.petri.hlpn.RBRHlNet.RBRPlaceId.*
import hydrozoa.integration.rbr.model.petri.hlpn.RBRHlNet.{Ballot, BallotStatus, RBRPlaceId, RBRTransitionId}
import hydrozoa.lib.classification.Histogram
import hydrozoa.lib.petri.hlpn.HlNet
import hydrozoa.rulebased.ledger.l1.state.VoteState.VoteStatus
import hydrozoa.rulebased.ledger.l1.utxo.BallotBox
import scalus.cardano.ledger.{Utxo, Utxos}

/** The on-chain-observable projection of an RBR state, shared by the Petri model and the L1 SUT:
  * token counts for the Dot / reference / output places, plus the `Ballots` place projected to
  * `(status, version)` (so status changes — Vote/Abstain/Ratchet — stay visible even though they
  * don't change the ballot count).
  *
  * Per-step bisimulation is `alpha(net) == beta(utxos)`. Places with no reliable L1 counterpart
  * (`Owner`, `VotingOpen`/`VotingClosed`, `ResolvedVersion`, `PayoutObligations`, `Collateral`,
  * `Ambient`) are excluded — see [[RBRClassifier]] for the collateral caveat.
  */
final case class ObservableMarking(
    counts: Map[RBRPlaceId, Int],
    ballots: Map[(BallotStatus, BigInt), Int]
)

object ObservableMarking:

    /** The observable count places. `Ballots` is projected separately; the rest of the HLNet places
      * have no distinct L1 UTxO and are excluded from reconciliation.
      */
    val countPlaces: List[RBRPlaceId] =
        List(
          UnresolvedTreasury,
          ResolvedTreasury,
          RegimeRef,
          DisputeScriptRef,
          TreasuryScriptRef,
          SetupLadder,
          EvacuationOutput
        )

    /** `alpha`: the Petri model's marking, projected. */
    def alpha(net: HlNet[RBRPlaceId, RBRTransitionId, Any]): ObservableMarking =
        val counts = countPlaces.map(p => p -> tokenCount(net, p)).toMap
        val ballots = net
            .marking(Ballots)
            .multiplicityMap
            .toList
            .groupMapReduce((tok, _) =>
                val b = tok.asInstanceOf[Ballot]
                (b.status, b.versionMinor)
            )((_, n) => n.toInt)(_ + _)
        ObservableMarking(counts, ballots)

    /** `beta`: the L1 UTxO snapshot, projected. Reuses [[RBRClassifier]] for the count places and
      * `BallotBox.parse` for the `(status, version)` ballot projection. `Left` on ambiguous
      * classification (the classifier fns must stay disjoint).
      */
    def beta(utxos: Utxos)(using cfg: MultiNodeConfig): Either[String, ObservableMarking] =
        val classifier = new RBRClassifier
        val all        = utxos.toList.map((i, o) => Utxo(i, o))
        Histogram.empty(classifier).addAll(all).toEither match
            case Left(errs) => Left(s"ambiguous UTxO classification: ${errs.toList}")
            case Right(hist) =>
                val counts = countPlaces.map(p => p -> hist(p)).toMap
                val ballots = all
                    .filter(u => classifier.classify(u).contains(Ballots))
                    .flatMap(u =>
                        BallotBox
                            .parse(u)(using cfg.nodeConfigs.values.head)
                            .toOption
                            .map(bb => project(bb.ballotBoxOutput.status))
                    )
                    .groupMapReduce(identity)(_ => 1)(_ + _)
                Right(ObservableMarking(counts, ballots))

    private def tokenCount(net: HlNet[RBRPlaceId, RBRTransitionId, Any], p: RBRPlaceId): Int =
        net.marking(p).multiplicityMap.values.map(_.toInt).sum

    /** Map an on-chain [[VoteStatus]] to the model's `(BallotStatus, version)` projection. */
    private def project(status: VoteStatus): (BallotStatus, BigInt) =
        status match
            case VoteStatus.Voted(_, versionMinor) => (BallotStatus.Voted, versionMinor)
            case VoteStatus.Abstain                => (BallotStatus.Abstained, BigInt(0))
            case VoteStatus.AwaitingVote(_)        => (BallotStatus.Awaiting, BigInt(0))
