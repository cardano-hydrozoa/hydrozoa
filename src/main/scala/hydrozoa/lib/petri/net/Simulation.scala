package hydrozoa.lib.petri.net

import hydrozoa.lib.petri.net.components.*

trait Simulation[ArcId, PlaceId, TransitionId] {
    def getPlaceSimulation(placeId: PlaceId): Option[Place.Simulation]
    def getArcSimulation(arcId: ArcId): Option[Arc.Simulation]
    def getTransitionSimulations(transitionId: TransitionId): Option[Transition.Simulation]
}
