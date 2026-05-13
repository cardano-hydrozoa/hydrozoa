package hydrozoa.lib.petri

import cats.*
import cats.effect.*
import hydrozoa.lib.number.*
import hydrozoa.lib.petri.MapNet.ValidationFlag
import hydrozoa.lib.petri.net.*
import hydrozoa.lib.petri.net.components.*
import hydrozoa.lib.petri.net.components.Arc.ArcNoId
import hydrozoa.lib.petri.net.components.ArcSemantics.PT
import hydrozoa.lib.petri.net.components.Place.PlaceCapacity
import hydrozoa.lib.petri.net.components.Place.PlaceCapacity.Bounded
import io.circe.*
import io.circe.syntax.*
import scala.collection.immutable.TreeMap

/** A net that is compatible with YAPNE serialization (i.e., stringly typed).
  * @param name
  * @param description
  * @param net
  * @param arcPresentations
  * @param placePresentations
  * @param transitionPresentations
  * @param dataVariablePresentations
  */

// TODO: If we have codecs for NetId, ArcId, PlaceId, and TransitionId, this can be done
//   using context bounds. This will be essential for typed nets to not need to rewrite codecs each time.
//   I'm just using this type alias for now to make my life easier
type YAPNENet = MapNet[
  String,
  String,
  String,
  String,
]

object YAPNENet {
    val yapneOps: MapNet.BuilderMOps[String, String, String, String] = MapNet.BuilderMOps()

    type YAPNEBuilder[PreValid <: ValidationFlag, PostValid <: ValidationFlag, A] =
        MapNet.BuilderM[String, String, String, String, A]

    given Encoder[MapNet[String, String, String, String]] with {

        def apply(yapne: YAPNENet): Json = {
            def placeJson(pid: String, place: PlaceNoId): Json =
                Json.obj(
                  "id" -> pid.asJson,
                  "position" -> Json.obj(
                    "x" -> place.position._1.asJson,
                    "y" -> place.position._2.asJson
                  ),
                  "label" -> place.label.asJson,
                  "tokens" -> place.tokens.asJson,
                  "capacity" -> (place.capacity match {
                      case PlaceCapacity.Unlimited   => Json.Null
                      case PlaceCapacity.Bounded(pi) => pi.convert.asJson
                  }),
                  "finalMarking" -> (place.finalMarking match {
                      case None      => Json.Null
                      case Some(nni) => nni.convert.asJson
                  }),
                  "radius" -> (place.radius.asJson)
                )

            def transitionJson(
                tid: String,
                transition: TransitionNoId,
            ): Json =
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
                  "precondition" -> "".asJson, // TODO,
                  "postcondition" -> "".asJson // TODO
                )

            def arcJson(
                arcId: String,
                arc: ArcNoId[String, String]
            ): Json = arc.semantics match {
                case pt: ArcSemantics.PT =>
                    Json.obj(
                      "id" -> arcId.asJson,
                      "source" -> arc.arcPlaceId.asJson,
                      "target" -> arc.arcTransitionId.asJson,
                      "weight" -> pt.weight.asJson,
                      "type" -> "regular".asJson,
                      "points" -> arc.points.asJson,
                      "label" -> arc.label.asJson
                    )
                case tp: ArcSemantics.TP =>
                    Json.obj(
                      "id" -> arcId.asJson,
                      "source" -> arc.arcTransitionId.asJson,
                      "target" -> arc.arcPlaceId.asJson,
                      "weight" -> tp.weight.asJson,
                      "type" -> "regular".asJson,
                      "points" -> arc.points.asJson,
                      "label" -> arc.label.asJson
                    )
                case i: ArcSemantics.Inhibitor.type =>
                    Json.obj(
                      "id" -> arcId.asJson,
                      "source" -> arc.arcPlaceId.asJson,
                      "target" -> arc.arcTransitionId.asJson,
                      // I DONT KNOW IF THIS WILL WORK, it might make things crash
                      "weight" -> Json.Null,
                      "type" -> "inhibitor".asJson,
                      "points" -> arc.points.asJson,
                      "label" -> arc.label.asJson
                    )
                case rd: ArcSemantics.Read =>
                    Json.obj(
                      "id" -> arcId.asJson,
                      "source" -> arc.arcPlaceId.asJson,
                      "target" -> arc.arcTransitionId.asJson,
                      "weight" -> Json.Null,
                      "type" -> "read".asJson,
                      "points" -> arc.points.asJson,
                      "label" -> arc.label.asJson
                    )
                case reset: ArcSemantics.Reset.type =>
                    Json.obj(
                      "id" -> arcId.asJson,
                      "source" -> arc.arcPlaceId.asJson,
                      "target" -> arc.arcTransitionId.asJson,
                      "weight" -> Json.Null,
                      "type" -> "reset".asJson,
                      "points" -> arc.points.asJson,
                      "label" -> arc.label.asJson
                    )
            }

            Json.obj(
              "id" -> yapne.id.asJson,
              "name" -> yapne.name.asJson,
              "description" -> yapne.description.asJson,
              "places" -> yapne.placesMap.toList.map((id, place) => placeJson(id, place)).asJson,
              "transitions" -> yapne.transitionsMap.toList
                  .map((id, transition) => transitionJson(id, transition))
                  .asJson,
              "arcs" -> yapne.arcsMap.toList.map((id, arc) => arcJson(id, arc)).asJson
            )
        }
    }
}

/** Demo showing the isolated Evacuation TxF
  */
object Demo extends IOApp {

    import YAPNENet.yapneOps.All.*
    import YAPNENet.given_Encoder_MapNet
    import MapNet.BuilderM.given

    object Places {
        val treasuryRef: PlaceNoId = PlaceNoId.unsafeApply(
          "TreasuryRef",
          tokens = NonNegativeInt.unsafeApply(1),
          capacity = Bounded(PositiveInt.unsafeApply(1)),
          finalMarking = Some(NonNegativeInt.unsafeApply(1))
        )

        val resolved: PlaceNoId = PlaceNoId.unsafeApply(
          "Resolved",
          tokens = NonNegativeInt.unsafeApply(1),
          capacity = Bounded(PositiveInt.unsafeApply(1)),
          finalMarking = Some(NonNegativeInt.unsafeApply(1))
        )

        val ambient: PlaceNoId = PlaceNoId.unsafeApply(
          "Ambient",
          tokens = NonNegativeInt.unsafeApply(100)
        )

        val payoutObligations: PlaceNoId = PlaceNoId.unsafeApply(
          "$PayoutObligations$",
          tokens = NonNegativeInt.unsafeApply(500),
          finalMarking = Some(NonNegativeInt.unsafeApply(0))
        )

        val evacuationOutput: PlaceNoId = PlaceNoId.unsafeApply(
          "EvacuationOutput",
          tokens = NonNegativeInt.unsafeApply(0),
          finalMarking = Some(NonNegativeInt.unsafeApply(500))
        )

        val collateral: PlaceNoId = PlaceNoId.unsafeApply(
          "Collateral",
          tokens = NonNegativeInt.unsafeApply(5),
          finalMarking = Some(NonNegativeInt.unsafeApply(5))
        )

        val map: TreeMap[String, PlaceNoId] = TreeMap.from(
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
        val evacuating: TransitionNoId = TransitionNoId(
          "Evacuating"
        )

        val map: TreeMap[String, TransitionNoId] =
            TreeMap.from(List(evacuating).zipWithIndex.map((t, index) => (s"t_$index", t)))
    }

    object Arcs {
        val readTreasury: ArcNoId[String, String] = Arc.ArcNoId(
          arcPlaceId = "p_0",
          arcTransitionId = "t_0",
          semantics = ArcSemantics.Read(NonNegativeInt.unsafeApply(1)),
          label = "reference treasury script (1)"
        )

        val spendResolved: ArcNoId[String, String] = Arc.ArcNoId(
          arcPlaceId = "p_1",
          arcTransitionId = "t_0",
          semantics = ArcSemantics.PT(NonNegativeInt.unsafeApply(1)),
          label = "spend resolved treasury (1)"
        )

        val sendResolved: ArcNoId[String, String] = Arc.ArcNoId(
          arcPlaceId = "p_1",
          arcTransitionId = "t_0",
          semantics = ArcSemantics.TP(NonNegativeInt.unsafeApply(1)),
          label = "send resolved treasury (1)"
        )

        val spendFeeUtxos: ArcNoId[String, String] = Arc.ArcNoId(
          arcPlaceId = "p_2",
          arcTransitionId = "t_0",
          semantics = PT(NonNegativeInt.unsafeApply(3)),
          label = "spend fee utxos (3)"
        )

        val sendChangeUtxo: ArcNoId[String, String] = Arc.ArcNoId(
          arcPlaceId = "p_2",
          arcTransitionId = "t_0",
          semantics = ArcSemantics.TP(NonNegativeInt.unsafeApply(1)),
          label = "send change utxo (1)"
        )

        val fulfillPayoutObligation: ArcNoId[String, String] = Arc.ArcNoId(
          arcPlaceId = "p_3",
          arcTransitionId = "t_0",
          semantics = ArcSemantics.PT(NonNegativeInt.unsafeApply(63)),
          label = "fulfill payout obligations (63)"
        )

        val sendEvacuationOutput: ArcNoId[String, String] = ArcNoId(
          arcPlaceId = "p_4",
          arcTransitionId = "t_0",
          semantics = ArcSemantics.TP(NonNegativeInt.unsafeApply(63)),
          label = "send evacuation output (63)"
        )

        val useCollateral: ArcNoId[String, String] = ArcNoId(
          arcPlaceId = "p_5",
          arcTransitionId = "t_0",
          semantics = ArcSemantics.Read(NonNegativeInt.unsafeApply(1)),
          label = "collateralize (1)"
        )

        val map: TreeMap[String, ArcNoId[String, String]] =
            TreeMap.from(
              List(
                readTreasury,
                spendResolved,
                sendResolved,
                spendFeeUtxos,
                sendChangeUtxo,
                fulfillPayoutObligation,
                sendEvacuationOutput,
                useCollateral
              ).zipWithIndex.map((a, index) => (s"a_$index", a))
            )

    }

    val evacuationNet: YAPNENet = {
        val builder
            : YAPNENet.YAPNEBuilder[ValidationFlag.Valid.type, ValidationFlag.Valid.type, Unit] =
            for {
                _ <- Foldable[List].sequence_(
                  Places.map.toList.map((pid, place) => createPlace(pid, place))
                )
                _ <- Foldable[List].sequence_(
                  Transitions.map.toList.map((tid, transition) => createTransition(tid, transition))
                )
                _ <- Foldable[List].sequence_(
                  Arcs.map.toList.map((arcId, arc) => createArc(arcId, arc))
                )
            } yield ()

        val Right(res) = builder.build("Evacuation Net", "")
        res._1
    }

    override def run(args: List[String]): IO[ExitCode] =
        for {
            _ <- IO.println(evacuationNet.asJson.noSpaces)
        } yield ExitCode.Success

}
