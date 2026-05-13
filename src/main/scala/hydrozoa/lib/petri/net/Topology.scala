package hydrozoa.lib.petri.net

import hydrozoa.lib.petri.*
import hydrozoa.lib.petri.net.components.*

object Topology {
    enum ValidationError[ArcId, PlaceId, TransitionId]:
        case DanglingArcPlace(arcId: ArcId, placeId: PlaceId)
        case DanglingArcTransition(arcId: ArcId, transitionId: TransitionId)
        case MissingArcTopology(arcId: ArcId)
}

/** Different representations of the net serve different purposes, but [[Valid]] nets MUST be
  * isomorphic. For example, the MapNet is a simple representation for building a net, while matrix
  * representations are more suitable for fast simulation and analysis
  *
  * When implementing this trait, you should override many of the methods for efficiency.
  */
// TODO: YAPNE (at least in its serialization) combines the topology of the net with the marking.
// I feel that these should be separated, such that the places should not carry tokens and the
// data variables should not carry actual values (just types). But this is easier for now, so I'll stick with it
// until I have more experience to inform the future layout
trait Topology[
    ArcId,
    PlaceId,
    TransitionId
] {

    def getArcTopology(arcId: ArcId): Option[Arc.Topology[PlaceId, TransitionId]]
    def getPlaceTopology(placeId: PlaceId): Option[Place.Topology]
    def getTransitionTopology(transitionId: TransitionId): Option[Transition.Topology]
}
//
//extension [NetId, ArcId, PlaceId, TransitionId] (t : Topology[ArcId, PlaceId, TransitionId] & Id[NetId, ArcId, PlaceId, TransitionId]) {
//    def validateTopologies
//}

object TopologyBuilder
