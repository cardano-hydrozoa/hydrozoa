//package hydrozoa.integration.rbr.model.petri.net
//
//import cats.data.*
//import cats.*
//import cats.syntax.all.*
//import cats.syntax.applicative.*
//import hydrozoa.config.head.HeadConfig
//import hydrozoa.integration.rbr.model.petri.net.Places.CollateralModel.CollateralModelError.WrongNumberOfCollateralOutputs
//import hydrozoa.integration.rbr.model.petri.net.Places.Resolved.Error.TreasuryNotResolved
//import hydrozoa.lib.cardano.scalus.ledger.CollateralOutput
//import hydrozoa.lib.number.{NonNegativeInt, PositiveInt}
//import hydrozoa.lib.petri.net.components.Place
//import hydrozoa.lib.petri.net.components.Place.Semantics
//import hydrozoa.lib.petri.net.components.Place.Semantics.MarkingError
//import hydrozoa.multisig.ledger.joint.EvacuationMap
//import hydrozoa.multisig.ledger.joint.obligation.Payout
//import hydrozoa.rulebased.ledger.l1.utxo.RuleBasedTreasuryUtxo
//import scalus.cardano.address.ShelleyAddress
//import scalus.cardano.address.ShelleyPaymentPart.Key
//import scalus.cardano.ledger.{TransactionOutput, Utxo, Utxos}
//
//object Places {
//
//    /** A place where tokens carry data, and duplicate tokens are allowed.
//      */
//    trait MultisetPlace[MarkingType, Self <: MultisetPlace[MarkingType, Self]]
//        extends Place.Topology,
//          Place.Syntax[Self],
//          Place.Semantics[Self],
//          Place.Syntax.HasTokens[Self] {
//        self: Self =>
//        final override type PlaceMarking = Map[MarkingType, PositiveInt]
//
//        final override val tokens: NonNegativeInt =
//            NonNegativeInt.unsafeApply(marking.values.map(_.toInt).sum)
//    }
//
//    extension [A](seq: Seq[A]) {
//        // WARNING: May overflow
//        def toMultiset: Map[A, PositiveInt] =
//            seq.foldLeft(Map.empty[A, PositiveInt])((acc, a) =>
//                acc.updatedWith(a) {
//                    case None    => Some(PositiveInt.unsafeApply(1))
//                    case Some(i) => Some(PositiveInt.unsafeApply(i.toInt + 1))
//                }
//            )
//    }
//
//    /** A place that must always have exactly one token. Used for reference script utxos, for
//      * example. Update functions are no-ops.
//      */
//    trait UnitPlace[MarkingType, Self <: UnitPlace[MarkingType, Self]]
//        extends Place.Topology,
//          Place.Syntax.HasTokens[Self],
//          Place.Syntax.HasFinalMarking[Self],
//          Place.Semantics[Self] {
//        self: Self =>
//
//        final override type PlaceMarking = MarkingType
//
//        final override def tokens: NonNegativeInt = NonNegativeInt.unsafeApply(1)
//
//    }
//
//    object Resolved {
//        enum Error extends MarkingError:
//            case TreasuryNotResolved
//            case WrongNumberOfTreasuryTokens(actual: Int)
//            case WrongNumberOfVoteTokens(expected: PositiveInt, actual: Int)
//    }
//
//    final case class Resolved(
//        override val marking: RuleBasedTreasuryUtxo,
//        override val finalMarking: Option[RuleBasedTreasuryUtxo]
//    )(using hc: HeadConfig)
//        extends UnitPlace[RuleBasedTreasuryUtxo, Resolved],
//          Place.Syntax.HasFinalMarking[Resolved] {
//
//        override def markingEndos(newMarking: RuleBasedTreasuryUtxo): List[Resolved => Resolved] =
//            List(_ => this.copy(marking = newMarking))
//
//        /** Note: because the RulebasedTreasuryUtxo constructor isn't private (see the TODO), we
//          * check it here instead.
//          *
//          * Conditions:
//          *   - Datum must be resolved
//          *   - numHeadPeers + 1 vote tokens
//          *   - 1 treasury token
//          *
//          * @return
//          */
//        override def markingErrors: List[MarkingError] = {
//            import Resolved.Error.*
//
//            val wrongDatum =
//                if marking.treasuryOutput.datum.isInstanceOf[Resolved]
//                then List.empty
//                else {
//                    List(TreasuryNotResolved)
//                }
//
//            val assets = marking.treasuryOutput.value.assets.assets
//
//            val wrongTokens =
//                val policyId = hc.headMultisigScript.policyId
//                val treasuryTn = hc.headTokenNames.treasuryTokenName
//                val voteTn = hc.headTokenNames.voteTokenName
//                val expectedVoteTokens = PositiveInt.unsafeApply(hc.nHeadPeers + 1)
//
//                assets match {
//                    case _ if !assets.contains(policyId) =>
//                        List(
//                          WrongNumberOfVoteTokens(
//                            actual = 0,
//                            expected = expectedVoteTokens
//                          ),
//                          WrongNumberOfTreasuryTokens(actual = 0)
//                        )
//                    case assets =>
//                        val voteErrors = assets(policyId).get(voteTn) match {
//                            case None =>
//                                List(
//                                  WrongNumberOfVoteTokens(actual = 0, expected = expectedVoteTokens)
//                                )
//                            case Some(n) if n != expectedVoteTokens.toLong =>
//                                List(
//                                  WrongNumberOfVoteTokens(actual = n, expected = expectedVoteTokens)
//                                )
//                            case _ => List.empty
//                        }
//
//                        val treasuryErrors = assets(policyId).get(treasuryTn) match {
//                            case None              => List(WrongNumberOfTreasuryTokens(actual = 0))
//                            case Some(n) if n != 1 => List(WrongNumberOfTreasuryTokens(actual = n))
//                            case _                 => List.empty
//                        }
//
//                        voteErrors ++ treasuryErrors
//                }
//            wrongDatum ++ wrongTokens
//        }
//    }
//
//    /** "Ambient" utxos, pre-existing in the utxo set. Fee utxos, change utxos, etc.
//      */
//    final case class Ambient(override val marking: Utxos) extends UnitPlace[Utxos, Ambient] {
//
//        override def markingEndos(newMarking: Utxos): List[Ambient => Ambient] =
//            List(_ => this.copy(marking = newMarking))
//    }
//
//    final case class PayoutObligationModel(
//        override val marking: Map[Payout.Obligation, PositiveInt],
//        //        override val finalMarking : Option[Map[Payout.Obligation, PositiveInt]]
//    ) extends MultisetPlace[Payout.Obligation, PayoutObligationModel] {
//
//        override def markingEndos(
//            newMarking: Map[Payout.Obligation, PositiveInt]
//        ): List[PayoutObligationModel => PayoutObligationModel] =
//            List(_ => this.copy(marking = newMarking))
//    }
//
//    /** Evacuation outputs get parameterized on an evacuation map
//      *
//      * @param evacuationMap
//      */
//    final case class EvacuationMapModel(evacuationMap: EvacuationMap) {
//
//        /** The full evacuation map, represented as a count of each duplicate transaction output
//          */
//        private val asMultiset: Map[TransactionOutput, PositiveInt] =
//            evacuationMap.evacuationMap.values.map(_.utxo.value).toSeq.toMultiset
//
//        enum EvacuationError extends Semantics.MarkingError:
//            case EvacuationOutputNotInMap(output: TransactionOutput)
//            case TooManyEvacuationOutputs(
//                output: TransactionOutput,
//                maxNumber: PositiveInt,
//                actualNumber: PositiveInt
//            )
//
//            override def getMessage: String = this match {
//                case EvacuationOutputNotInMap(output) =>
//                    s"The evacuation output $output is not in the evacuation map"
//                case TooManyEvacuationOutputs(output, max, actual) =>
//                    s"The evacuation output $output is in the evacuation map, but " +
//                        s"it only contains $max copies. We found $actual outputs matching that signature."
//            }
//
//        /** @param marking
//          * @param finalMarking
//          *   In most cases, we'll want this to be the fully-evacuated output set.
//          */
//        final case class EvacuationOutput(
//            override val marking: Map[TransactionOutput, PositiveInt],
//            override val finalMarking: Option[Map[TransactionOutput, PositiveInt]] = Some(
//              asMultiset
//            )
//        ) extends MultisetPlace[TransactionOutput, EvacuationOutput],
//              Place.Syntax.HasFinalMarking[EvacuationOutput] {
//
//            override protected def markingEndos(
//                newMarking: Map[TransactionOutput, PositiveInt]
//            ): List[EvacuationOutput => EvacuationOutput] =
//                List(_ => this.copy(marking = newMarking))
//
//            /** This encodes the fact that the transaction outputs marked in this Place must be
//              * members of the evacuation map.
//              *
//              * @return
//              */
//            override def markingErrors: List[MarkingError] = {
//
//                marking.foldLeft(List.empty) {
//                    case (errors, (output, _n)) if !asMultiset.keySet.contains(output) =>
//                        EvacuationError.EvacuationOutputNotInMap(output) :: errors
//                    case (errors, (output, n)) if asMultiset(output) < n =>
//                        EvacuationError.TooManyEvacuationOutputs(
//                          output,
//                          asMultiset(output),
//                          n
//                        ) :: errors
//                    case (errors, _) => errors
//                }
//            }
//        }
//    }
//
//    object CollateralModel {
//        enum CollateralModelError extends MarkingError:
//            case WrongNumberOfCollateralOutputs(expected: PositiveInt, actual: Int)
//            case WrongCollateralAddress(actualAddress: ShelleyAddress)
//
//            override def getMessage: String = this match {
//                case WrongNumberOfCollateralOutputs(expected, actual) =>
//                    s"Found an unexpected number of collateral outputs. Expected: $expected. Actual: $actual"
//                case WrongCollateralAddress(addr) =>
//                    s"The candidate output does not belong to any peer. The actual address is $addr"
//            }
//    }
//
//    final case class Collateral(marking: Map[CollateralOutput, PositiveInt])(using hc: HeadConfig)
//        extends MultisetPlace[CollateralOutput, Collateral] {
//        override protected def markingEndos(
//            newMarking: Map[CollateralOutput, PositiveInt]
//        ): List[Collateral => Collateral] =
//            List(_ => this.copy(marking = newMarking))
//
//        /** Collateral markings must be of the right arity (no more and no less than the number of
//          * peers) and must be at the correct addresses
//          * @return
//          */
//        override def markingErrors: List[MarkingError] = {
//            val wrongNumError = {
//                val expected = hc.headPeers.nHeadPeers
//                val actual = marking.values.map(_.toInt).sum
//                if expected.toInt != actual
//                then List(WrongNumberOfCollateralOutputs(expected, actual))
//                else List.empty
//            }
//            val wrongAddresses = {
//                marking.foldLeft(List.empty[MarkingError]) { case (errors, (output, _n)) =>
//                    val actualAddr: ShelleyAddress =
//                        output.toOutput.address.asInstanceOf[ShelleyAddress]
//                    val validAddresses = hc.headPeerAddresses.toSortedMap.values.toSet
//
//                    if validAddresses.contains(actualAddr)
//                    then List.empty
//                    else
//                        List(
//                          CollateralModel.CollateralModelError.WrongCollateralAddress(actualAddr)
//                        )
//                }
//            }
//
//            wrongNumError ++ wrongAddresses
//        }
//    }
//}
