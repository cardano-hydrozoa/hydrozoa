package hydrozoa.lib.petri.net

import hydrozoa.lib.petri.net.components.*

trait Configuration[ArcId, PlaceId, TransitionId] {
    def getPlaceConfiguration(id: PlaceId): Option[Place.Semantics]
    def getTransitionConfigurations(id: TransitionId): Option[Transition.Semantics]
    def getArcConfiguration(id: ArcId): Option[Arc.Semantics]
}
