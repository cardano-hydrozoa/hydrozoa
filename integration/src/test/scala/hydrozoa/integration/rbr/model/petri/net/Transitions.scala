package hydrozoa.integration.rbr.model.petri.net

object Transitions {
    enum RBRTransitionId:
        case EvacuationId

    object RBRTransitionId {
        given Ordering[RBRTransitionId] = Ordering.by(_.ordinal)
    }
}
