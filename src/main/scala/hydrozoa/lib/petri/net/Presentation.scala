package hydrozoa.lib.petri.net

import hydrozoa.lib.petri.net.components.*

/** A presentation associated with a net. This tells us the position, labels, and size of the
  * elements of the net.
  */
trait Presentation[
    ArcId,
    PlaceId,
    TransitionId,
] {
    // TODO: Maybe these should be polymorphic. It will matter when we start combining subnets into larger nets
    def name: String

    def description: String

    def getPlacePresentations(id: PlaceId): Option[Place.Presentation]
    def arcPresentations(id: ArcId): Option[Arc.Presentation]
    def transitionPresentations(id: TransitionId): Option[Transition.Presentation]

}
