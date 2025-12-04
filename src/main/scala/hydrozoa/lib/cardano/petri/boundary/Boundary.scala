package hydrozoa.lib.cardano.petri.boundary

import Boundary.*

trait Boundary[SourceId, TargetId, Place]
    extends Has.SourceId[SourceId],
      Has.TargetId[TargetId],
      Has.Place[Place]

object Boundary {

    /** The source side of a boundary, not yet connected to the target side. */
    trait SourceSide[SourceId, Place] extends Has.SourceId[SourceId], Has.Place[Place] {
        type B <: Boundary[SourceId, TargetId, Place]
        type TargetId
        def attachTarget(targetId: TargetId): B
    }

    /** The target side of a boundary, not yet connected to the source side. */
    trait TargetSide[TargetId, Place] extends Has.TargetId[TargetId], Has.Place[Place] {
        type B <: Boundary[SourceId, TargetId, Place]
        type SourceId
        def attachSource[B <: Boundary[SourceId, TargetId, Place], SourceId]: B
    }

    /** A place that can be attached at a given side of a boundary. Useful when constructing the
      * place, before attaching it to the boundary side.
      */
    trait Detached[Id, Place] {
        opaque type DetachedType = Place

        type S <: SourceSide[Id, Place] | TargetSide[Id, Place]

        def apply(x: Place): DetachedType = x

        def attach(detached: DetachedType, id: Id): S

        extension (detached: DetachedType) def place: Place = detached
    }

    /** A boundary side (either source or target) that masks its source/target ID. It behaves
      * similarly to a detached place, until it is unmasked.
      */
    trait Masked[S <: SourceSide[SourceId, Place] | TargetSide[SourceId, Place], SourceId, Place] {
        opaque type MaskedType = S

        def apply(x: S): MaskedType = x

        extension (self: MaskedType)
            def unmask: S = self
            def place: Place = self.place
    }

    object Has {
        trait Place[Place] {
            def place: Place
        }

        trait SourceId[SourceId] {
            def sourceId: SourceId
        }

        trait TargetId[TargetId] {
            def targetId: TargetId
        }
    }
}
