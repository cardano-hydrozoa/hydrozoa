package hydrozoa.lib.petri

import hydrozoa.lib.petri.ArcType.given
import hydrozoa.lib.petri.Net.*
import hydrozoa.lib.petri.Place.{PlaceCapacity, PlacePresentation}
import hydrozoa.lib.petri.Transition.TransitionPresentation
import io.circe.*
import io.circe.syntax.*

/** A net that is compatible with YAPNE serialization (i.e., stringly typed and valid).
  * @param name
  * @param description
  * @param net
  * @param arcPresentations
  * @param placePresentations
  * @param transitionPresentations
  * @param dataVariablePresentations
  */
trait YAPNENet
    extends Net.Presentation[
      String,
      String,
      String,
      String,
      String,
      String,
      String,
      String,
      ValidationFlag.Valid.type
    ]

object YAPNENet {

    type YAPNEBuilder[PreValid <: ValidationFlag, PostValid <: ValidationFlag] =
        MapNet.BuilderM[String, String, String, String, String, PreValid, PostValid]

    given Encoder[YAPNENet] with {
        private def placePresentationJson(
            place: Place[String],
            placePresentation: PlacePresentation[String]
        ): Json =
            Json.obj(
              "id" -> place.id.asJson,
              "position" -> Json.obj(
                "x" -> placePresentation.position._1.asJson,
                "y" -> placePresentation.position._2.asJson
              ),
              "label" -> placePresentation.label.asJson,
              "tokens" -> place.tokens.asJson,
              "capacity" -> (place.capacity match {
                  case PlaceCapacity.Unlimited   => Json.Null
                  case PlaceCapacity.Bounded(pi) => pi.convert.asJson
              }),
              "finalMarking" -> (place.finalMarking match {
                  case None      => Json.Null
                  case Some(nni) => nni.convert.asJson
              }),
              "radius" -> (placePresentation.radius.asJson)
            )

        private def transitionPresentationJson(
            transition: Transition[String],
            transitionPresentation: TransitionPresentation[String],
            enabledTransitions: Set[String]
        ): Json =
            Json.obj(
              "id" -> transition.id.asJson,
              "position" -> Json.obj(
                "x" -> transitionPresentation.position._1.asJson,
                "y" -> transitionPresentation.position._2.asJson
              ),
              "label" -> transitionPresentation.label.asJson,
              "width" -> transitionPresentation.width.asJson,
              "height" -> transitionPresentation.height.asJson,
              "isEnabled" -> enabledTransitions.contains(transition.id).asJson,
              "priority" -> transitionPresentation.priority.asJson,
              "delay" -> transitionPresentation.delay.toMillis.asJson,
              "silent" -> transitionPresentation.silent.asJson,
              "precondition" -> "".asJson, // TODO,
              "postcondition" -> "".asJson // TODO
            )

        private def arcPresentationJson(
            arc: Arc[String, String, String],
            arcPresentation: ArcPresentation[String]
        ): Json = arc match {
            case pt: ArcPT[String, String, String] =>
                Json.obj(
                  "id" -> pt.id.asJson,
                  "source" -> pt.source.asJson,
                  "target" -> pt.target.asJson,
                  "weight" -> pt.weight.asJson,
                  "type" -> pt.arcType.asJson,
                  "points" -> arcPresentation.points.asJson,
                  "label" -> arcPresentation.label.asJson
                )
            case tp: ArcTP[String, String, String] =>
                Json.obj(
                  "id" -> tp.id.asJson,
                  "source" -> tp.source.asJson,
                  "target" -> tp.target.asJson,
                  "weight" -> tp.weight.asJson,
                  "type" -> tp.arcType.asJson,
                  "points" -> arcPresentation.points.asJson,
                  "label" -> arcPresentation.label.asJson
                )
        }

        private def dataVariablePresentationJson(
            dv: DataVariable[String],
            presentation: DataVariablePresentation
        ): Json = dv match {
            case DataVariable.Int(i, id) =>
                Json.obj(
                  "id" -> id.asJson,
                  "name" -> presentation.name.asJson,
                  "type" -> "int".asJson,
                  "currentValue" -> i.asJson,
                  "description" -> presentation.description.asJson
                )
            case DataVariable.Real(r, id) =>
                Json.obj(
                  "id" -> id.asJson,
                  "name" -> presentation.name.asJson,
                  "type" -> "real".asJson,
                  "currentValue" -> r.asJson,
                  "description" -> presentation.description.asJson
                )
            case DataVariable.Boolean(b, id) =>
                Json.obj(
                  "id" -> id.asJson,
                  "name" -> presentation.name.asJson,
                  "type" -> "boolean".asJson,
                  "currentValue" -> b.asJson,
                  "description" -> presentation.description.asJson
                )
        }

        def apply(yapne: YAPNENet): Json =
            Json.obj(
              "id" -> yapne.net.id.asJson,
              "name" -> yapne.name.asJson,
              "description" -> yapne.description.asJson,
              "places" -> yapne.net.places
                  .map(place =>
                      val pres = yapne.placePresentations(place.id)
                      placePresentationJson(place = place, placePresentation = pres)
                  )
                  .asJson,
              "transitions" -> yapne.net.transitions
                  .map(t =>
                      val pres = yapne.transitionPresentations(t.id)
                      transitionPresentationJson(t, pres, yapne.net.getEnabledTransitions)
                  )
                  .asJson,
              "arcs" -> yapne.net.arcs.map { arc =>
                  val pres = yapne.arcPresentations(arc.id)
                  arcPresentationJson(arc, pres)
              }.asJson,
              "dataVariables" -> yapne.net.dataVariables
                  .map(dv =>
                      val pres = yapne.dataVariablePresentations(dv.id)
                      dataVariablePresentationJson(dv, pres)
                  )
                  .asJson
            )
    }
}
