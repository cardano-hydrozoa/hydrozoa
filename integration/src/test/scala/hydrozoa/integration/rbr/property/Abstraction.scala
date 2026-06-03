package hydrozoa.integration.rbr.property

import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.integration.rbr.model.petri.net.RBRPlaceId
import hydrozoa.integration.rbr.model.petri.net.RBRPlaceId.*
import hydrozoa.lib.classification.{Classifier, Histogram}
import hydrozoa.rulebased.ledger.l1.state.TreasuryState.RuleBasedTreasuryDatum
import hydrozoa.rulebased.ledger.l1.state.VoteState.VoteStatus
import hydrozoa.rulebased.ledger.l1.utxo.{RuleBasedTreasuryUtxo, VoteUtxo}
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.{TransactionOutput, Utxo}
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.Data
import scalus.uplc.builtin.Data.toData

/** Histogram of every UTxO in the shared mock backend, bucketed by [[RBRPlaceId]].
  *
  * A type alias for [[Histogram]]`[RBRPlaceId, Utxo]`. Produced by running
  * [[RBRClassifier]] over a UTxO snapshot.
  */
type RBRHistogram = Histogram[RBRPlaceId, Utxo]

/** Classifies a UTxO snapshot into [[RBRPlaceId]] buckets for use in property tests.
  *
  * Classification is purely content-based (no transaction history required):
  *   - Treasury and vote UTxOs carry a distinctive policy token
  *   - Collateral UTxOs carry the datum sentinel `"collateral"`
  *   - Evacuation outputs carry the datum sentinel `"evacuation"`
  *   - Script reference UTxOs are identified by their known [[scalus.cardano.ledger.TransactionInput]] keys
  *   - Everything else falls into the [[AmbientPlaceId]] default bucket
  *
  * Usage: `Histogram.empty(RBRClassifier(using env)).addAll(utxos.map(Utxo(_, _)))`
  */
class RBRClassifier(using env: MultiNodeConfig)
    extends Classifier[RBRPlaceId, Utxo](AmbientPlaceId):

    def classifierFns: List[Utxo => Option[RBRPlaceId]] =
        val policyId              = env.headConfig.headMultisigScript.policyId
        val treasuryToken         = env.headConfig.headTokenNames.treasuryTokenName
        val voteToken             = env.headConfig.headTokenNames.voteTokenName
        val treasuryScriptInput   = env.headConfig.rulebasedTreasuryScriptInput
        val disputeScriptInput    = env.headConfig.disputeResolutionScriptInput
        val collateralDatumMarker = toData(ByteString.fromString("collateral"))
        val evacuationDatumMarker = toData(ByteString.fromString("evacuation"))

        def hasTreasuryToken(out: TransactionOutput): Boolean =
            out.value.assets.assets.get(policyId).exists(_.contains(treasuryToken))

        def hasVoteToken(out: TransactionOutput): Boolean =
            out.value.assets.assets.get(policyId).exists(_.contains(voteToken))

        def hasInlineDatum(marker: Data)(out: TransactionOutput): Boolean =
            out.datumOption.exists { case Inline(data) => data == marker; case _ => false }

        List(
            (u: Utxo) => Option.when(u.input == treasuryScriptInput)(TreasuryRefPlaceId),
            (u: Utxo) => Option.when(u.input == disputeScriptInput)(DisputeRefPlaceId),
            // ResolvedTreasuryPlaceId / UnresolvedTreasuryPlaceId: carries treasury token; parse datum to distinguish
            (u: Utxo) =>
                Option
                    .when(hasTreasuryToken(u.output))(u)
                    .flatMap(RuleBasedTreasuryUtxo.parse(_)(using env.nodeConfigs.values.head).toOption)
                    .map(tu =>
                        if tu.treasuryOutput.datum.isInstanceOf[RuleBasedTreasuryDatum.Resolved] then
                            ResolvedTreasuryPlaceId
                        else UnresolvedTreasuryPlaceId
                    ),
            // VotedPlaceId / UnvotedPlaceId: carries vote token; parse datum to distinguish
            (u: Utxo) =>
                Option
                    .when(hasVoteToken(u.output) && !hasTreasuryToken(u.output))(u)
                    .flatMap(VoteUtxo.parse(_)(using env.nodeConfigs.values.head).toOption)
                    .map(vu =>
                        if vu.voteOutput.status.isInstanceOf[VoteStatus.Voted] then VotedPlaceId
                        else UnvotedPlaceId
                    ),
            (u: Utxo) =>
                Option.when(hasInlineDatum(collateralDatumMarker)(u.output))(CollateralPlaceId),
            (u: Utxo) =>
                Option.when(hasInlineDatum(evacuationDatumMarker)(u.output))(
                    EvacuationOutputPlaceId
                ),
        )
