package hydrozoa.integration.rbr.property

import hydrozoa.config.ScriptReferenceUtxos
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.integration.rbr.model.petri.net.RBRPlaceId
import hydrozoa.integration.rbr.model.petri.net.RBRPlaceId.*
import hydrozoa.lib.cardano.scalus.VerificationKeyExtra.addrKeyHash
import hydrozoa.lib.cardano.scalus.ledger.{CollateralUtxo, CollateralOutput}
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.rulebased.ledger.l1.state.VoteState.VoteStatus
import hydrozoa.rulebased.ledger.l1.utxo.{RuleBasedTreasuryOutput, RuleBasedTreasuryUtxo, VoteUtxo}
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.address.ShelleyPaymentPart.Key
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.{AddrKeyHash, TransactionInput, TransactionOutput, Utxo, Utxos}
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.Data.toData

/** Classification of every UTxO in the shared mock backend after dispute resolution.
  *
  * Unlike [[hydrozoa.integration.rbr.model.petri.net.EvacuationAbstraction]], which requires
  * transaction structure (reference/collateral inputs) to disambiguate, this classifier works
  * purely from UTxO content — address + carried tokens + datum. That is sufficient here because:
  *   - Treasury and vote UTxOs carry a distinctive policy token with a parseable inline datum
  *   - Collateral UTxOs carry the UTF-8 bytestring `"collateral"` as their datum (test convention
  *     from [[hydrozoa.rulebased.ledger.l1.tx.CommonGenerators.genCollateralUtxo]])
  *   - Reference script UTxOs are identified by their known [[TransactionInput]] keys from the env
  */
case class DisputeClassification(
    // Parsed treasury UTxO (None if consumed by resolution)
    treasury: Option[RuleBasedTreasuryUtxo],
    // All vote UTxOs still at the dispute address (empty after full resolution)
    votes: List[VoteUtxo[VoteStatus]],
    // Per-peer ADA-only collateral UTxOs, keyed by [[HeadPeerNumber]]
    collaterals: Map[HeadPeerNumber, CollateralUtxo],
    // Typed treasury script reference UTxO (unchanged by dispute resolution)
    treasuryScriptRef: Option[ScriptReferenceUtxos.TreasuryScriptUtxo],
    // Typed dispute resolution script reference UTxO (unchanged by dispute resolution)
    disputeScriptRef: Option[ScriptReferenceUtxos.DisputeScriptUtxo],
    // Everything that did not match any known category
    ambient: Utxos
)

object DisputeAbstraction {

    sealed trait ClassificationError extends Throwable

    object ClassificationError {
        /** A UTxO matched more than one category predicate — indicates a test invariant violation. */
        case class AmbiguousUTxO(input: TransactionInput, places: List[RBRPlaceId])
            extends ClassificationError {
            override def getMessage: String =
                s"UTxO $input matched multiple places: ${places.mkString(", ")}"
        }

        /** A UTxO carries the treasury token but its datum cannot be parsed. */
        case class TreasuryParseFailure(input: TransactionInput, cause: String)
            extends ClassificationError {
            override def getMessage: String =
                s"UTxO $input carries the treasury token but failed to parse: $cause"
        }

        /** A UTxO carries the vote token but its datum cannot be parsed. */
        case class VoteParseFailure(input: TransactionInput, cause: VoteUtxo.ParseError)
            extends ClassificationError {
            override def getMessage: String =
                s"UTxO $input carries the vote token but failed to parse: ${cause.getMessage}"
        }

        /** A UTxO has the collateral datum sentinel but could not be projected to a CollateralUtxo. */
        case class CollateralParseFailure(input: TransactionInput, cause: CollateralUtxo.ParseError)
            extends ClassificationError {
            override def getMessage: String =
                s"UTxO $input has the collateral datum but failed to parse: ${cause.getMessage}"
        }

        /** A known ref-script input failed script-hash or network validation. */
        case class RefScriptParseFailure(input: TransactionInput, cause: ScriptReferenceUtxos.Error)
            extends ClassificationError {
            override def getMessage: String =
                s"UTxO $input is a known ref-script input but failed to parse: ${cause.getMessage}"
        }
    }

    // Sentinel datum used by CommonGenerators.genCollateralUtxo to mark test collateral UTxOs.
    private val collateralDatumMarker = toData(ByteString.fromString("collateral"))

    /** Classify the full UTxO snapshot purely from content (no transaction history needed).
      *
      * Each UTxO is tested against all categories independently; matching more than one is an error.
      * Category tags use [[RBRPlaceId]] for consistency with the rest of the RBR model:
      *   - [[TreasuryRefPlaceId]]         — input key is the treasury script ref input
      *   - [[DisputeRefPlaceId]]          — input key is the dispute resolution script ref input
      *   - [[ResolvedTreasuryPlaceId]]    — output carries the treasury token
      *   - [[VotedPlaceId]]               — output carries the vote token
      *   - [[CollateralPlaceId]]          — output has the collateral datum sentinel
      *   - [[AmbientPlaceId]]             — fallback (no category matched)
      *
      * Carrying the policy token but failing to parse is a hard error, not an ambient fallback.
      */
    def classify(
        utxos: Utxos
    )(using env: MultiNodeConfig): Either[ClassificationError, DisputeClassification] = {

        // All peers share the same headConfig; any NodeConfig satisfies the parse config constraints
        given voteParseConfig: VoteUtxo.ParseConfig          = env.nodeConfigs.values.head
        given treasuryConfig: RuleBasedTreasuryOutput.Config = env.nodeConfigs.values.head

        val policyId      = env.headConfig.headMultisigScript.policyId
        val treasuryToken = env.headConfig.headTokenNames.treasuryTokenName
        val voteToken     = env.headConfig.headTokenNames.voteTokenName

        // Known ref-script input keys from the env (same across all peers)
        val treasuryScriptInput  = env.headConfig.rulebasedTreasuryScriptInput
        val disputeScriptInput   = env.headConfig.disputeResolutionScriptInput

        // Reverse map: peer pubkey-address key hash → HeadPeerNumber
        val peerByKeyHash: Map[AddrKeyHash, HeadPeerNumber] =
            env.nodePrivateConfigs.map { (peerId, cfg) =>
                cfg.ownHeadPeerPrivate.ownHeadWallet.exportVerificationKey.addrKeyHash -> peerId
            }

        def hasTreasuryToken(out: TransactionOutput): Boolean =
            out.value.assets.assets.get(policyId).exists(_.contains(treasuryToken))

        def hasVoteToken(out: TransactionOutput): Boolean =
            out.value.assets.assets.get(policyId).exists(_.contains(voteToken))

        def hasCollateralDatum(out: TransactionOutput): Boolean =
            out.datumOption.exists {
                case Inline(data) => data == collateralDatumMarker
                case _            => false
            }

        def peerOf(out: TransactionOutput): Option[HeadPeerNumber] =
            out.address match {
                case sa: ShelleyAddress =>
                    sa.payment match {
                        case Key(hash) => peerByKeyHash.get(AddrKeyHash(hash))
                        case _         => None
                    }
                case _ => None
            }

        utxos.foldLeft(
          Right(
            DisputeClassification(
              treasury = None,
              votes = Nil,
              collaterals = Map.empty,
              treasuryScriptRef = None,
              disputeScriptRef = None,
              ambient = Map.empty
            )
          ): Either[ClassificationError, DisputeClassification]
        ) { case (accE, (input, out)) =>
            accE.flatMap { acc =>
                val isTreasuryScriptRef = input == treasuryScriptInput
                val isDisputeScriptRef  = input == disputeScriptInput
                val isTreasury          = hasTreasuryToken(out)
                val isVote              = hasVoteToken(out) && !hasTreasuryToken(out)
                val isCollateral        = hasCollateralDatum(out)

                // Tag each matched category using RBRPlaceId for ambiguity detection
                val matchedPlaces: List[RBRPlaceId] = List(
                  Option.when(isTreasuryScriptRef)(TreasuryRefPlaceId),
                  Option.when(isDisputeScriptRef)(DisputeRefPlaceId),
                  Option.when(isTreasury)(ResolvedTreasuryPlaceId),
                  Option.when(isVote)(VotedPlaceId),
                  Option.when(isCollateral)(CollateralPlaceId)
                ).flatten

                matchedPlaces match {
                    case Nil =>
                        Right(acc.copy(ambient = acc.ambient + (input -> out)))

                    case _ :: Nil =>
                        if isTreasuryScriptRef then
                            ScriptReferenceUtxos.TreasuryScriptUtxo(env.headConfig, Utxo(input, out))
                                .map(u => acc.copy(treasuryScriptRef = Some(u)))
                                .left.map(ClassificationError.RefScriptParseFailure(input, _))
                        else if isDisputeScriptRef then
                            ScriptReferenceUtxos.DisputeScriptUtxo(env.headConfig, Utxo(input, out))
                                .map(u => acc.copy(disputeScriptRef = Some(u)))
                                .left.map(ClassificationError.RefScriptParseFailure(input, _))
                        else if isTreasury then
                            RuleBasedTreasuryUtxo
                                .parse(Utxo(input, out))
                                .map(t => acc.copy(treasury = Some(t)))
                                .left.map(e =>
                                    ClassificationError.TreasuryParseFailure(input, e.getMessage)
                                )
                        else if isVote then
                            VoteUtxo
                                .parse(Utxo(input, out))
                                .map(vu => acc.copy(votes = vu :: acc.votes))
                                .left.map(ClassificationError.VoteParseFailure(input, _))
                        else // isCollateral
                            CollateralUtxo
                                .parse(Utxo(input, out))
                                .map { cu =>
                                    val peer = peerOf(out).getOrElse(
                                      throw RuntimeException(
                                        s"Collateral UTxO $input has collateral datum but no matching peer address"
                                      )
                                    )
                                    acc.copy(collaterals = acc.collaterals + (peer -> cu))
                                }
                                .left.map(ClassificationError.CollateralParseFailure(input, _))

                    case multiple =>
                        Left(ClassificationError.AmbiguousUTxO(input, multiple))
                }
            }
        }
    }
}
