package hydrozoa.lib.petri

import cats.*
import cats.effect.*
import cats.syntax.all.*
import hydrozoa.lib.number.*
import hydrozoa.lib.petri.net.*
import hydrozoa.lib.petri.net.components.*
import hydrozoa.lib.petri.net.components.Place.Semantics.Bounded.Error.TooManyTokens
import io.circe.*
import io.circe.syntax.*
import scala.collection.immutable.{Queue, TreeMap}

// =============================================================================
// YapnePlace — a unified place type for YAPNE nets
// =============================================================================

/** A place type compatible with YAPNE serialization. Capacity is optional: `None` means unlimited
  * (like [[UnboundedPlace]]); `Some(bound)` enforces a token ceiling (like [[BoundedPlace]]).
  * `finalMarking` carries simulator semantics (it is the target marking for reachability /
  * termination checks), but those semantics are not yet implemented.
  */
case class YapnePlace(
    override val label: String,
    override val marking: NonNegativeInt = NonNegativeInt.unsafeApply(0),
    val capacity: Option[PositiveInt] = None,
    override val finalMarking: Option[NonNegativeInt] = None,
    override val position: (Int, Int) = (0, 0),
    override val radius: PositiveInt = PositiveInt.unsafeApply(20),
) extends Place.Topology,
      Place.Syntax.HasFinalMarking[YapnePlace],
      Place.Semantics[YapnePlace],
      Place.Presentation {

    override type PlaceMarking = NonNegativeInt
    override def mark(n: NonNegativeInt): YapnePlace = this.copy(marking = n)
    override def withFinalMarking(m: Option[NonNegativeInt]): YapnePlace =
        this.copy(finalMarking = m)

    override def markingError: Option[Place.Semantics.MarkingError] =
        capacity.flatMap(bound =>
            Option.when(marking.toInt > bound.toInt)(TooManyTokens(marking, bound))
        )
}

// =============================================================================
// YapneArc — sealed hierarchy, one subtype per YAPNE arc kind
// =============================================================================

/** Sealed arc type for YAPNE nets. Pattern-match on the concrete subtype to determine the arc kind
  * when encoding or decoding.
  */
sealed trait YapneArc
    extends Arc.Topology[String, String],
      Arc.Syntax,
      Arc.Semantics[YapnePlace],
      Arc.Presentation

/** Place-to-transition arc. Removes `weight` tokens from the place on firing. */
case class YapnePTArc(
    override val arcPlaceId: String,
    override val arcTransitionId: String,
    override val weight: PositiveInt,
    override val label: String,
    override val points: Queue[(Int, Int)] = Queue.empty,
) extends YapneArc,
      Arc.Semantics.PT[YapnePlace]

/** Transition-to-place arc. Adds `weight` tokens to the place on firing. */
case class YapneTPArc(
    override val arcPlaceId: String,
    override val arcTransitionId: String,
    override val weight: PositiveInt,
    override val label: String,
    override val points: Queue[(Int, Int)] = Queue.empty,
) extends YapneArc,
      Arc.Semantics.TP[YapnePlace]

/** Inhibitor arc. Enabled only when the place is empty; does not consume tokens. */
case class YapneInhibitorArc(
    override val arcPlaceId: String,
    override val arcTransitionId: String,
    override val label: String,
    override val points: Queue[(Int, Int)] = Queue.empty,
) extends YapneArc,
      Arc.Semantics.Inhibitor[YapnePlace]

/** Reset arc. Drains all tokens from the place on firing; always arc-side enabled. */
case class YapneResetArc(
    override val arcPlaceId: String,
    override val arcTransitionId: String,
    override val label: String,
    override val points: Queue[(Int, Int)] = Queue.empty,
) extends YapneArc,
      Arc.Semantics.Reset[YapnePlace]

/** Read arc. Enabled when the place has >= `weight` tokens; does not consume tokens. */
case class YapneReadArc(
    override val arcPlaceId: String,
    override val arcTransitionId: String,
    override val weight: PositiveInt,
    override val label: String,
    override val points: Queue[(Int, Int)] = Queue.empty,
) extends YapneArc,
      Arc.Semantics.Read[YapnePlace]

// =============================================================================
// YapneNet — type alias and companion
// =============================================================================

// TODO: If we have codecs for ArcId, PlaceId, and TransitionId, this can be done
//   using context bounds. This will be essential for typed nets to not need to rewrite codecs each
//   time. Using String IDs here matches the YAPNE JSON schema.
type YapneNet = MapNet[String, String, String, YapneArc, YapnePlace, TransitionNoId]

object YapneNet {

    val yapneOps: MapNet.BuilderMOps[String, String, String, YapneArc, YapnePlace, TransitionNoId] =
        MapNet.BuilderMOps()

    type YapneBuilder[R] =
        MapNet.BuilderM[String, String, String, YapneArc, YapnePlace, TransitionNoId, R]

    given Encoder[YapneNet] with {

        def apply(yapne: YapneNet): Json = {

            def placeJson(pid: String, place: YapnePlace): Json =
                Json.obj(
                  "id" -> pid.asJson,
                  "position" -> Json.obj(
                    "x" -> place.position._1.asJson,
                    "y" -> place.position._2.asJson
                  ),
                  "label" -> place.label.asJson,
                  "tokens" -> place.marking.asJson,
                  "capacity" -> (place.capacity match {
                      case None     => Json.Null
                      case Some(pi) => pi.asJson
                  }),
                  "finalMarking" -> (place.finalMarking match {
                      case None      => Json.Null
                      case Some(nni) => nni.asJson
                  }),
                  "radius" -> place.radius.asJson
                )

            def transitionJson(tid: String, transition: TransitionNoId): Json =
                Json.obj(
                  "id" -> tid.asJson,
                  "position" -> Json.obj(
                    "x" -> transition.position._1.asJson,
                    "y" -> transition.position._2.asJson
                  ),
                  "label" -> transition.label.asJson,
                  "width" -> transition.width.asJson,
                  "height" -> transition.height.asJson,
                  "isEnabled" -> yapne.isEnabled(tid).asJson,
                  "priority" -> transition.priority.asJson,
                  "delay" -> transition.delay.toMillis.asJson,
                  "silent" -> transition.silent.asJson,
                  "precondition" -> "".asJson, // TODO
                  "postcondition" -> "".asJson // TODO
                )

            def arcJson(arcId: String, arc: YapneArc): Json = arc match {
                case pt: YapnePTArc =>
                    Json.obj(
                      "id" -> arcId.asJson,
                      "source" -> arc.arcPlaceId.asJson,
                      "target" -> arc.arcTransitionId.asJson,
                      "weight" -> pt.weight.asJson,
                      "type" -> "regular".asJson,
                      "points" -> arc.points.toList.asJson,
                      "label" -> arc.label.asJson
                    )
                case tp: YapneTPArc =>
                    Json.obj(
                      "id" -> arcId.asJson,
                      "source" -> arc.arcTransitionId.asJson,
                      "target" -> arc.arcPlaceId.asJson,
                      "weight" -> tp.weight.asJson,
                      "type" -> "regular".asJson,
                      "points" -> arc.points.toList.asJson,
                      "label" -> arc.label.asJson
                    )
                case _: YapneInhibitorArc =>
                    Json.obj(
                      "id" -> arcId.asJson,
                      "source" -> arc.arcPlaceId.asJson,
                      "target" -> arc.arcTransitionId.asJson,
                      // I DONT KNOW IF THIS WILL WORK, it might make things crash
                      "weight" -> Json.Null,
                      "type" -> "inhibitor".asJson,
                      "points" -> arc.points.toList.asJson,
                      "label" -> arc.label.asJson
                    )
                case rd: YapneReadArc =>
                    Json.obj(
                      "id" -> arcId.asJson,
                      "source" -> arc.arcPlaceId.asJson,
                      "target" -> arc.arcTransitionId.asJson,
                      "weight" -> Json.Null,
                      "type" -> "read".asJson,
                      "points" -> arc.points.toList.asJson,
                      "label" -> arc.label.asJson
                    )
                case _: YapneResetArc =>
                    Json.obj(
                      "id" -> arcId.asJson,
                      "source" -> arc.arcPlaceId.asJson,
                      "target" -> arc.arcTransitionId.asJson,
                      "weight" -> Json.Null,
                      "type" -> "reset".asJson,
                      "points" -> arc.points.toList.asJson,
                      "label" -> arc.label.asJson
                    )
            }

            Json.obj(
              "places" -> yapne.placesMap.toList.map((id, place) => placeJson(id, place)).asJson,
              "transitions" -> yapne.transitionsMap.toList
                  .map((id, transition) => transitionJson(id, transition))
                  .asJson,
              "arcs" -> yapne.arcsMap.toList.map((id, arc) => arcJson(id, arc)).asJson
            )
        }
    }
}

// =============================================================================
// Demo — Evacuation TxF net
// =============================================================================

/** Demo showing the isolated Evacuation TxF
  */
object Demo extends IOApp {

    import Net.Semantics.FinalMarking.*
    import YapneNet.yapneOps.*
    import MapNet.BuilderM.given

    object Places {
        val treasuryRef: YapnePlace = YapnePlace(
          label = "TreasuryRef",
          marking = NonNegativeInt.unsafeApply(1),
          capacity = Some(PositiveInt.unsafeApply(1)),
          finalMarking = Some(NonNegativeInt.unsafeApply(1))
        )

        val resolved: YapnePlace = YapnePlace(
          label = "Resolved",
          marking = NonNegativeInt.unsafeApply(1),
          capacity = Some(PositiveInt.unsafeApply(1)),
          finalMarking = Some(NonNegativeInt.unsafeApply(1))
        )

        val ambient: YapnePlace = YapnePlace(
          label = "Ambient",
          marking = NonNegativeInt.unsafeApply(100),
          finalMarking = Some(NonNegativeInt.unsafeApply(86)) // 100 - 7×2
        )

        val payoutObligations: YapnePlace = YapnePlace(
          label = "$PayoutObligations$",
          marking = NonNegativeInt.unsafeApply(500),
          finalMarking =
              Some(NonNegativeInt.unsafeApply(59)) // 500 - 7×63; deadlocks here (59 < 63)
        )

        val evacuationOutput: YapnePlace = YapnePlace(
          label = "EvacuationOutput",
          marking = NonNegativeInt.unsafeApply(0),
          finalMarking = Some(NonNegativeInt.unsafeApply(441)) // 7×63
        )

        val collateral: YapnePlace = YapnePlace(
          label = "Collateral",
          marking = NonNegativeInt.unsafeApply(5),
          finalMarking = Some(NonNegativeInt.unsafeApply(5))
        )

        val map: TreeMap[String, YapnePlace] = TreeMap.from(
          List(
            treasuryRef,
            resolved,
            ambient,
            payoutObligations,
            evacuationOutput,
            collateral
          ).zipWithIndex.map((place, index) => (s"p_$index", place))
        )
    }

    object Transitions {
        val evacuating: TransitionNoId = TransitionNoId("Evacuating")

        val map: TreeMap[String, TransitionNoId] =
            TreeMap.from(List(evacuating).zipWithIndex.map((t, index) => (s"t_$index", t)))
    }

    object Arcs {
        val readTreasury: YapneArc = YapneReadArc(
          arcPlaceId = "p_0",
          arcTransitionId = "t_0",
          weight = PositiveInt.unsafeApply(1),
          label = "reference treasury script (1)"
        )

        // PT(1)+TP(1) on p_1 collapses to a net-zero change — equivalent to a read.
        val readResolved: YapneArc = YapneReadArc(
          arcPlaceId = "p_1",
          arcTransitionId = "t_0",
          weight = PositiveInt.unsafeApply(1),
          label = "read resolved treasury (1)"
        )

        // PT(3)+TP(1) on p_2 collapses to a net -2 — equivalent to a single PT(2).
        val spendAmbient: YapneArc = YapnePTArc(
          arcPlaceId = "p_2",
          arcTransitionId = "t_0",
          weight = PositiveInt.unsafeApply(2),
          label = "spend fee utxos net (2)"
        )

        val fulfillPayoutObligation: YapneArc = YapnePTArc(
          arcPlaceId = "p_3",
          arcTransitionId = "t_0",
          weight = PositiveInt.unsafeApply(63),
          label = "fulfill payout obligations (63)"
        )

        val sendEvacuationOutput: YapneArc = YapneTPArc(
          arcPlaceId = "p_4",
          arcTransitionId = "t_0",
          weight = PositiveInt.unsafeApply(63),
          label = "send evacuation output (63)"
        )

        val useCollateral: YapneArc = YapneReadArc(
          arcPlaceId = "p_5",
          arcTransitionId = "t_0",
          weight = PositiveInt.unsafeApply(1),
          label = "collateralize (1)"
        )

        val map: TreeMap[String, YapneArc] =
            TreeMap.from(
              List(
                readTreasury,
                readResolved,
                spendAmbient,
                fulfillPayoutObligation,
                sendEvacuationOutput,
                useCollateral
              ).zipWithIndex.map((a, index) => (s"a_$index", a))
            )
    }

    val evacuationNet: YapneNet = {
        val builder: YapneNet.YapneBuilder[Unit] =
            for {
                _ <- Places.map.toList.traverse_ { case (pid, place) => addPlace(pid, place) }
                _ <- Transitions.map.toList.traverse_ { case (tid, t) => addTransition(tid, t) }
                _ <- Arcs.map.toList.traverse_ { case (aid, arc) => addArc(aid, arc) }
            } yield ()

        val Right((net, _)) = builder.runEmpty: @unchecked
        net
    }

    override def run(args: List[String]): IO[ExitCode] =
        for {
            vs <- IO.fromEither(
              ValidatedSimulator
                  .validate(evacuationNet)
                  .leftMap(errs =>
                      new RuntimeException(
                        errs.toList.map(_.getMessage).mkString("; ")
                      )
                  )
            )
            _ <- IO.println("Net is valid.")
            // Fire t_0 repeatedly until it is no longer enabled.
            finalVs = Iterator
                .iterate(vs)(_.fire("t_0").fold(throw _, identity))
                .dropWhile(_.isEnabled("t_0"))
                .next()
            _ <- IO.println(
              s"Simulation complete. Enabled transitions: ${finalVs.enabledTransitions}"
            )
            finalNet = finalVs.get
            _ <- IO.println(
              s"Terminal errors:\n${finalNet.terminalErrors.map("  " + _.getMessage).mkString("\n")}"
            )
            _ <- IO.println(s"Is valid terminal: ${finalNet.isValidTerminal}")
        } yield ExitCode.Success

}
