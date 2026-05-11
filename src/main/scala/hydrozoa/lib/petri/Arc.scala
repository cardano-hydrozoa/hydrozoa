package hydrozoa.lib.petri

import hydrozoa.lib.number.NonNegativeInt
import io.circe.{Codec, Decoder, Encoder}
import scala.collection.immutable.Queue

// I think it's easy to mess up the ordering of these type variables...
// Is there syntax to name them better?
sealed trait Arc[ArcId, TransitionId, PlaceId] {
    val id: ArcId
    val weight: NonNegativeInt
    val arcType: ArcType
    def getPlaceId: PlaceId
    def getTransitionId: TransitionId
}

case class ArcPT[ArcId, TransitionId, PlaceId](
    source: PlaceId,
    target: TransitionId,
    override val id: ArcId,
    override val weight: NonNegativeInt,
    override val arcType: ArcType
) extends Arc[ArcId, TransitionId, PlaceId] {
    override def getPlaceId: PlaceId = source

    override def getTransitionId: TransitionId = target
}

case class ArcTP[ArcId, TransitionId, PlaceId](
    source: TransitionId,
    target: PlaceId,
    override val id: ArcId,
    override val weight: NonNegativeInt,
    override val arcType: ArcType
) extends Arc[ArcId, TransitionId, PlaceId] {
    override def getPlaceId: PlaceId = target

    override def getTransitionId: TransitionId = source
}

// N.B.:  The arc always starts from the source and ends at the target. These are additional points that go along
// the way. I don't believe this is integrated into the YAPNE GUI yet.
case class ArcPresentation[Label](points: Queue[(Int, Int)], label: Label)

// N.B.: On the YAPNE GUI, it appears that only the "Regular" arc type is available?
enum ArcType:
    case Regular
    case Inhibitor
    case Reset
    case Read

object ArcType {
    // TODO: I don't know the encoding for the others right now, I'll have to look at the source
    given Codec[ArcType] = Codec.from(
      decodeA = Decoder.decodeString.emap { case "normal" =>
          Right(Regular)
      },
      encodeA = Encoder.encodeString.contramap { case Regular =>
          "normal"
      }
    )

}
