package hydrozoa.integration.rbr.model.petri.net

import hydrozoa.integration.rbr.model.petri.net.RBRPlaceId.*
import hydrozoa.integration.rbr.model.petri.net.transitions.EvacuationNet
import hydrozoa.lib.number.NonNegativeInt
import hydrozoa.multisig.ledger.joint.EvacuationMap
import hydrozoa.rulebased.ledger.l1.state.TreasuryState.RuleBasedTreasuryDatum
import hydrozoa.rulebased.ledger.l1.utxo.RuleBasedTreasuryOutput
import scalus.cardano.ledger.{Transaction, TransactionInput, TransactionOutput, Utxos}

object EvacuationAbstraction {

    sealed trait AbstractionError {
        def getMessage: String
    }

    object AbstractionError {

        /** A reference input parses as an Unresolved treasury UTxO. During evacuation only Resolved
          * or script-ref UTxOs should appear as reference inputs.
          */
        case class UnresolvedTreasuryRefInput(key: TransactionInput) extends AbstractionError {
            override def getMessage: String =
                s"Reference input $key parses as Unresolved treasury; expected Resolved or script ref"
        }

        /** A UTxO key matched more than one classification predicate. */
        case class AmbiguousClassification(key: TransactionInput, tags: List[RBRPlaceId])
            extends AbstractionError {
            override def getMessage: String =
                s"UTxO $key matched multiple places: ${tags.mkString(", ")}"
        }
    }

    // NOTE: All transactions are currently treated uniformly when deriving reference inputs,
    // collateral inputs, and produced outputs. In the full model test this should be refined
    // to filter transactions by their metadata tag (e.g. EvacuationTx) so that inputs/outputs
    // from other transaction types do not accidentally pollute the classification sets.

    /** Classify a UTxO set into an [[EvacuationNet]] marking.
      *
      * Place membership is derived from transaction structure rather than UTxO content alone,
      * to correctly handle UTxOs whose on-chain shape is indistinguishable from unrelated UTxOs:
      *   - Reference inputs → [[TreasuryRefPlaceId]] or [[ResolvedPlaceId]] (by content)
      *   - Collateral inputs → [[CollateralPlaceId]]
      *   - Transaction outputs matching `evacuationMap.cooked` → [[EvacuationOutputPlaceId]]
      *   - Everything else → [[AmbientPlaceId]]
      *   - `evacuationMap.size` → [[PayoutObligationsPlaceId]] (synthetic, no UTxO)
      */
    def apply(
        utxos: Utxos,
        evacuationMap: EvacuationMap,
        transactions: List[Transaction],
    )(using config: RuleBasedTreasuryOutput.Config): Either[AbstractionError, Map[RBRPlaceId, NonNegativeInt]] = {

        val referenceKeys: Set[TransactionInput] =
            transactions.flatMap(_.body.value.referenceInputs.toSet).toSet

        val collateralKeys: Set[TransactionInput] =
            transactions.flatMap(_.body.value.collateralInputs.toSet).toSet

        val evacuationOutputValues: Set[TransactionOutput] =
            evacuationMap.cooked.values.toSet

        val evacuationOutputKeys: Set[TransactionInput] =
            transactions.flatMap { tx =>
                tx.body.value.outputs.toList.zipWithIndex.collect {
                    case (sized, ix) if evacuationOutputValues.contains(sized.value) =>
                        TransactionInput(tx.id, ix)
                }
            }.toSet

        // Pre-validate: no reference input may have an Unresolved datum during evacuation.
        val unresolvedRefError: Option[AbstractionError] =
            referenceKeys.collectFirst {
                case key if utxos.get(key).exists(out =>
                        RuleBasedTreasuryOutput(out).exists(
                          _.datum.isInstanceOf[RuleBasedTreasuryDatum.Unresolved]
                        )
                    ) =>
                    AbstractionError.UnresolvedTreasuryRefInput(key)
            }

        // One predicate per place (except Ambient, which is the fallback).
        // Each returns Some(placeId) if the UTxO key matches; None otherwise.
        // Order does not affect correctness — a UTxO matching >1 predicate is an error.
        val predicates: List[(TransactionInput, TransactionOutput) => Option[RBRPlaceId]] = List(
            (key, out) =>
                Option.when(referenceKeys.contains(key)) {
                    // After pre-validation, any parseable treasury ref input is Resolved.
                    if RuleBasedTreasuryOutput(out).isRight then ResolvedPlaceId
                    else TreasuryRefPlaceId
                },
            (key, _) => Option.when(collateralKeys.contains(key))(CollateralPlaceId),
            (key, _) => Option.when(evacuationOutputKeys.contains(key))(EvacuationOutputPlaceId),
        )

        def classifyOne(key: TransactionInput, output: TransactionOutput): Either[AbstractionError, RBRPlaceId] =
            predicates.flatMap(_(key, output)) match {
                case Nil        => Right(AmbientPlaceId)
                case tag :: Nil => Right(tag)
                case tags       => Left(AbstractionError.AmbiguousClassification(key, tags))
            }

        val classified: Either[AbstractionError, Map[RBRPlaceId, Int]] =
            unresolvedRefError.toLeft(()).flatMap { _ =>
                utxos.foldLeft(Right(Map.empty): Either[AbstractionError, Map[RBRPlaceId, Int]]) {
                    case (acc, (key, output)) =>
                        for {
                            counts  <- acc
                            placeId <- classifyOne(key, output)
                        } yield counts.updatedWith(placeId)(n => Some(n.getOrElse(0) + 1))
                }
            }

        classified.map { counts =>
            RBRPlaceId.values.map { placeId =>
                val count =
                    if placeId == PayoutObligationsPlaceId then evacuationMap.size
                    else counts.getOrElse(placeId, 0)
                placeId -> NonNegativeInt.unsafeApply(count)
            }.toMap
        }
    }

    /** True iff the UTxO set and evacuation map are consistent with the net's current marking. */
    def equivalent(
        utxos: Utxos,
        evacuationMap: EvacuationMap,
        transactions: List[Transaction],
        net: EvacuationNet,
    )(using config: RuleBasedTreasuryOutput.Config): Either[AbstractionError, Boolean] =
        apply(utxos, evacuationMap, transactions).map(
            _ == net.placesMap.view.mapValues(_.marking).toMap
        )
}
