package hydrozoa.lib.petri

import cats.data.{Kleisli, NonEmptyList}
import hydrozoa.lib.number.NonNegativeInt
import hydrozoa.lib.petri.MapNet.BuilderError.*
import hydrozoa.lib.petri.Net.{TopologyAndMarking, ValidationFlag}
import scala.collection.immutable.TreeMap

/** A map representation without presentation data. We use this for building typed nets. TreeMap is
  * selected internally to assist in serialization stability (I guess this isn't strictly needed,
  * but it will make storing json artifacts in version control easier. Should we relax it?).
  *
  * There are many type parameters here. It's a better choice that using path-dependent types
  * bundled in a trait because it makes inference easier. It's recommended not to use this directly
  * because it is verbose; instead, make wrapper types or aliases.
  */
case class MapNet[
    NetId,
    PlaceId: Ordering,
    TransitionId: Ordering,
    ArcId: Ordering,
    DataVariableId: Ordering,
    VF <: ValidationFlag
] private (
    override val id: NetId,
    // Technically there ends up being duplication of IDs. We could reduce this, but I don't think
    // its worth it right now
    placesMap: TreeMap[PlaceId, Place[PlaceId]],
    transitionsMap: TreeMap[TransitionId, Transition[TransitionId]],
    arcsMap: TreeMap[ArcId, Arc[ArcId, TransitionId, PlaceId]],
    dataVariablesMap: TreeMap[DataVariableId, DataVariable[DataVariableId]]
) extends TopologyAndMarking[NetId, PlaceId, TransitionId, ArcId, DataVariableId, VF] {

    override lazy val places: Seq[Place[PlaceId]] = placesMap.values.toSeq
    override lazy val transitions: Seq[Transition[TransitionId]] = transitionsMap.values.toSeq
    override lazy val arcs: Seq[Arc[ArcId, TransitionId, PlaceId]] = arcsMap.values.toSeq
    override lazy val dataVariables: Seq[DataVariable[DataVariableId]] =
        dataVariablesMap.values.toSeq

    /** Helper method to strip the validation
      */
    private[petri] def invalidate: MapNet[
      NetId,
      PlaceId,
      TransitionId,
      ArcId,
      DataVariableId,
      ValidationFlag.Invalid.type
    ] =
        new MapNet[
          NetId,
          PlaceId,
          TransitionId,
          ArcId,
          DataVariableId,
          ValidationFlag.Invalid.type
        ](
          id = id,
          placesMap = placesMap,
          transitionsMap = transitionsMap,
          arcsMap = arcsMap,
          dataVariablesMap = dataVariablesMap
        )
}

object MapNet {

    enum ValidationError[ArcId, PlaceId, TransitionId]:
        case DanglingPlace(
            arcid: ArcId,
            dangling: PlaceId
        )
        case DanglingTransition(
            ardId: ArcId,
            dangling: TransitionId
        )

    extension [
        NetId,
        PlaceId: Ordering,
        TransitionId: Ordering,
        ArcId: Ordering,
        DataVariableId: Ordering
    ](
        invalid: MapNet[
          NetId,
          PlaceId,
          TransitionId,
          ArcId,
          DataVariableId,
          ValidationFlag.Invalid.type
        ]
    ) {

        /** Check for:
          *   - Dangling Arcs
          *   - TODO: Syntactically correct transition pre/post conditions
          */
        def validate: Either[NonEmptyList[ValidationError[ArcId, PlaceId, TransitionId]], MapNet[
          NetId,
          PlaceId,
          TransitionId,
          ArcId,
          DataVariableId,
          ValidationFlag.Valid.type
        ]] = {
            val arcIdsNormalized: List[(ArcId, PlaceId, TransitionId)] =
                invalid.arcsMap
                    .map((arcId, arc) => (arcId, arc.getPlaceId, arc.getTransitionId))
                    .toList

            val tids = invalid.transitionsMap.keySet
            val pids = invalid.placesMap.keySet

            val errors = arcIdsNormalized.foldLeft(
              List.empty[ValidationError[ArcId, PlaceId, TransitionId]]
            ) {
                case (errors, (_, pid, tid)) if pids.contains(pid) && tids.contains(tid) =>
                    errors
                case (errors, (aid, pid, tid)) if !pids.contains(pid) && tids.contains(tid) =>
                    errors.prepended(ValidationError.DanglingPlace(aid, pid))
                case (errors, (aid, pid, tid)) if pids.contains(pid) && !tids.contains(tid) =>
                    errors.prepended(ValidationError.DanglingTransition(aid, tid))
                case (errors, (aid, pid, tid)) =>
                    errors
                        .prepended(ValidationError.DanglingTransition(aid, tid))
                        .prepended(ValidationError.DanglingPlace(aid, pid))
            }

            if errors.isEmpty
            then
                Right(
                  new MapNet[
                    NetId,
                    PlaceId,
                    TransitionId,
                    ArcId,
                    DataVariableId,
                    ValidationFlag.Valid.type
                  ](
                    id = invalid.id,
                    placesMap = invalid.placesMap,
                    transitionsMap = invalid.transitionsMap,
                    arcsMap = invalid.arcsMap,
                    dataVariablesMap = invalid.dataVariablesMap
                  )
                )
            else Left(NonEmptyList.fromListUnsafe(errors))
        }
    }

    def empty[
        NetId,
        PlaceId: Ordering,
        TransitionId: Ordering,
        ArcId: Ordering,
        DataVariableId: Ordering,
    ](id: NetId): MapNet[
      NetId,
      PlaceId,
      TransitionId,
      ArcId,
      DataVariableId,
      ValidationFlag.Valid.type
    ] =
        new MapNet(
          id = id,
          placesMap = TreeMap.empty,
          transitionsMap = TreeMap.empty,
          arcsMap = TreeMap.empty,
          dataVariablesMap = TreeMap.empty
        )

    enum BuilderError[PlaceId, TransitionId, ArcId, DataVariableId]:
        case PlaceIdConflict(placeId: PlaceId)
        case PlaceIdMissing(placeId: PlaceId)
        case TransitionIdConflict(transitionId: TransitionId)
        case TransitionIdMissing(transitionId: TransitionId)
        case ArcIdConflict(arcId: ArcId)
        case ArcIdMissing(arcId: ArcId)
        case DataVariableIdConflict(dataVariableId: DataVariableId)
        case DataVariableIdMissing(dataVariableId: DataVariableId)

    /** The BuilderKleisli type gives maximum flexibility for constructing typed nets.
      *
      * The PreValid and PostValid types allow for constraining whether elements (arcs, transitions,
      * placesMap, variables) must be added in order, or if "dangling" elements can be added and
      * resolved later.
      */
    private type BuilderKleisli[
        NetId,
        PlaceId,
        TransitionId,
        ArcId,
        DataVariableId,
        PreValid <: ValidationFlag,
        PostValid <: ValidationFlag
    ] = Kleisli[
      [X] =>> Either[BuilderError[PlaceId, TransitionId, ArcId, DataVariableId], X],
      MapNet[NetId, PlaceId, TransitionId, ArcId, DataVariableId, PreValid],
      MapNet[NetId, PlaceId, TransitionId, ArcId, DataVariableId, PostValid]
    ]

    case class BuilderM[
        NetId,
        PlaceId: Ordering,
        TransitionId: Ordering,
        ArcId: Ordering,
        DataVariableId: Ordering,
        PreValid <: ValidationFlag,
        PostValid <: ValidationFlag
    ] private (
        builder: BuilderKleisli[
          NetId,
          PlaceId,
          TransitionId,
          ArcId,
          DataVariableId,
          PreValid,
          PostValid
        ]
    )

    /** Helper case class to avoid repeatedly apply type parameters. You should probably wrap this
      * for your domain-specific nets.
      *
      * The method are split into four categories:
      *   - If a method will NOT require re-validation of the net (i.e., it will never change a
      *     validation flag from Valid to Invalid), it is in the [[Safe]] object. Otherwise, it is
      *     [[Unsafe]].
      *   - If a method verifies the existence (respectively non-existence) of an Id in the MapNet
      *     before updating/deleting (respectively, creating), it is named without an underscore and
      *     will return a Left(FooIdConflict) if this assumption is violated. Otherwise, it is named
      *     with an underscore, and the operation will be a no-op (resepectively, will overwrite).
      */
    case class BuilderMOps[
        NetId,
        PlaceId: Ordering,
        TransitionId: Ordering,
        ArcId: Ordering,
        DataVariableId: Ordering,
    ]() {

        private type MN[VF <: ValidationFlag] =
            MapNet[NetId, PlaceId, TransitionId, ArcId, DataVariableId, VF]

        private type BK[PreValid <: ValidationFlag, PostValid <: ValidationFlag] =
            BuilderKleisli[
              NetId,
              PlaceId,
              TransitionId,
              ArcId,
              DataVariableId,
              PreValid,
              PostValid
            ]
        private type P = Place[PlaceId]
        private type T = Transition[TransitionId]
        private type A = Arc[ArcId, TransitionId, PlaceId]
        private type DV = DataVariable[DataVariableId]

        private type BE = BuilderError[PlaceId, TransitionId, ArcId, DataVariableId]

        object All {
            export Safe.*
            export Unsafe.*
        }

        /** Methods in this object will always result in a net that has the same validation status
          * after the operation as it did before. In this sense, they are "safe" -- they will never
          * move a net from "Valid" to "Invalid"
          */
        object Safe {

            /** Create a place. If a place with the given ID already exists, this will return a
              * Left. See also [[createPlace_]]
              */
            def createPlace[InitialStatus <: ValidationFlag](
                place: P
            ): BK[InitialStatus, InitialStatus] =
                Kleisli(mapNet =>
                    val placeIds = mapNet.placesMap.keySet
                    if placeIds.contains(place.id)
                    then Left(PlaceIdConflict(place.id))
                    else
                        Right(
                          mapNet.copy(placesMap = mapNet.placesMap.updated(place.id, place))
                        )
                )

            /** Like [[createPlace]], but will override existing place IDs.
              */
            def createPlace_[InitialStatus <: ValidationFlag](
                place: P
            ): BK[InitialStatus, InitialStatus] = Kleisli(mapNet =>
                Right(mapNet.copy(placesMap = mapNet.placesMap.updated(place.id, place)))
            )

            /** Create a transition. Note that if a transition with the given ID already exists,
              * this will return a Left. See also [[createTransition_]]
              */
            def createTransition[InitialStatus <: ValidationFlag](
                transition: T
            ): BK[InitialStatus, InitialStatus] = Kleisli(mapNet =>
                val transitionIds = mapNet.transitionsMap.keySet
                if transitionIds.contains(transition.id)
                then Left(TransitionIdConflict(transition.id))
                else
                    Right(
                      mapNet.copy(transitionsMap =
                          mapNet.transitionsMap.updated(transition.id, transition)
                      )
                    )
            )

            /** Like [[createTransition]], but will override existing transition IDs.
              */
            def createTransition_[InitialStatus <: ValidationFlag](
                transition: T
            ): BK[InitialStatus, InitialStatus] = Kleisli(mapNet =>
                Right(
                  mapNet.copy(transitionsMap =
                      mapNet.transitionsMap.updated(transition.id, transition)
                  )
                )
            )
        }

        /** The methods in this object mirror the YAPNE API. They do not perform validation as they
          * go, and thus always set the PostValidation phantom type to False. It is useful for
          * building nets "out of order" and only validating at the end, specifically when adding
          * arcs where the given PlaceId or TransitionId may not yet exist.
          */
        object Unsafe {
            private type Invalidating = BK[ValidationFlag, ValidationFlag.Invalid.type]

            /** Why it's unsafe: we don't check whether the given source/target ID exists. Will
              * through an error if the given ArcId already exists. See also [[createArc_]]
              */
            def createArc(arc: A): Invalidating = Kleisli(mapNet =>
                val arcIds = mapNet.arcsMap.keySet
                if arcIds.contains(arc.id)
                then Left(ArcIdConflict(arc.id))
                else Right(mapNet.copy(arcsMap = mapNet.arcsMap.updated(arc.id, arc)))
            )

            def createArc_(arc: A): Invalidating =
                Kleisli(mapNet => Right(mapNet.copy(arcsMap = mapNet.arcsMap.updated(arc.id, arc))))

            def updateArc(id: ArcId)(
                f: (PlaceId, TransitionId, NonNegativeInt, ArcType) => A
            ): Invalidating =
                Kleisli(mapNet => {
                    val aids = mapNet.arcsMap.keySet
                    if aids.contains(id)
                    then {
                        val oldArc = mapNet.arcsMap(id)
                        val newArc = oldArc match {
                            case ArcPT(
                                  pid: PlaceId,
                                  tid: TransitionId,
                                  _aid,
                                  priority,
                                  arcType
                                ) =>
                                f(pid, tid, priority, arcType)
                            case ArcTP(
                                  tid: TransitionId,
                                  pid: PlaceId,
                                  _aid,
                                  priority,
                                  arcType
                                ) =>
                                f(pid, tid, priority, arcType)
                        }
                        Right(mapNet.copy(arcsMap = mapNet.arcsMap.updated(id, newArc)))
                    } else Left(ArcIdMissing(id))
                })

            def updateArc_(id: ArcId)(
                f: (PlaceId, TransitionId, NonNegativeInt, ArcType) => A
            ): Invalidating =
                Kleisli(mapNet => {
                    val aids = mapNet.arcsMap.keySet
                    if aids.contains(id)
                    then {
                        val oldArc = mapNet.arcsMap(id)
                        val newArc = oldArc match {
                            case ArcPT(
                                  pid: PlaceId,
                                  tid: TransitionId,
                                  _aid,
                                  priority,
                                  arcType
                                ) =>
                                f(pid, tid, priority, arcType)
                            case ArcTP(
                                  tid: TransitionId,
                                  pid: PlaceId,
                                  _aid,
                                  priority,
                                  arcType
                                ) =>
                                f(pid, tid, priority, arcType)
                        }
                        Right(mapNet.copy(arcsMap = mapNet.arcsMap.updated(id, newArc)))
                    } else Right(mapNet.invalidate)
                })

            def removeArc(aid: ArcId): Invalidating = Kleisli(mapNet =>
                val aids = mapNet.arcsMap.keySet
                if aids.contains(aid)
                then Right(mapNet.copy(arcsMap = mapNet.arcsMap.removed(aid)))
                else Left(ArcIdMissing(aid))
            )

            def removeArc_(aid: ArcId): Invalidating = Kleisli(mapNet =>
                val aids = mapNet.arcsMap.keySet
                if aids.contains(aid)
                then Right(mapNet.copy(arcsMap = mapNet.arcsMap.removed(aid)))
                else Right(mapNet.invalidate)
            )

            /** Update the data associated with the given place ID. If the ID is changed, this may
              * lead to dangling arcs. Will return an error if the given placeId is not found.
              */
            def updatePlace(id: PlaceId)(
                f: (NonNegativeInt, Place.PlaceCapacity, Option[NonNegativeInt]) => P
            ): Invalidating = Kleisli(mapNet =>
                val pids = mapNet.placesMap.keySet
                if pids.contains(id)
                then {
                    val oldPlace = mapNet.placesMap(id)
                    val newPlace =
                        f(oldPlace.tokens, oldPlace.capacity, oldPlace.finalMarking)
                    Right(mapNet.copy(placesMap = mapNet.placesMap.updated(id, newPlace)))
                } else Left(PlaceIdMissing(id))
            )

            /** Like [[updatePlace_]], but is a no-op if the given placeId is not found.
              */
            def updatePlace_(id: PlaceId)(
                f: (NonNegativeInt, Place.PlaceCapacity, Option[NonNegativeInt]) => P
            ): Invalidating = Kleisli(mapNet =>
                val pids = mapNet.placesMap.keySet
                if pids.contains(id)
                then {
                    val oldPlace = mapNet.placesMap(id)
                    val newPlace =
                        f(oldPlace.tokens, oldPlace.capacity, oldPlace.finalMarking)
                    Right(mapNet.copy(placesMap = mapNet.placesMap.updated(id, newPlace)))
                } else Right(mapNet.invalidate)
            )

            def removePlace(pid: PlaceId): Invalidating = Kleisli(mapNet =>
                val pids = mapNet.placesMap.keySet
                if pids.contains(pid)
                then Right(mapNet.copy(placesMap = mapNet.placesMap.removed(pid)))
                else Left(PlaceIdMissing(pid))
            )

            def removePlace_(pid: PlaceId): Invalidating = Kleisli(mapNet =>
                val pids = mapNet.placesMap.keySet
                if pids.contains(pid)
                then Right(mapNet.copy(placesMap = mapNet.placesMap.removed(pid)))
                else Right(mapNet.invalidate)
            )

            /** Update the data associated with the given TransitionID. If the ID is changed, this
              * may lead to dangling arcs. Will return an error if the given TransitionId is not
              * found.
              */
            def updateTransition(id: TransitionId)(
                f: (Expression, Expression) => T
            ): Invalidating = Kleisli(mapNet =>
                val tids = mapNet.transitionsMap.keySet
                if tids.contains(id)
                then {
                    val oldTransition = mapNet.transitionsMap(id)
                    val newTransition =
                        f(oldTransition.precondition, oldTransition.postcondition)
                    Right(
                      mapNet.copy(transitionsMap = mapNet.transitionsMap.updated(id, newTransition))
                    )
                } else Left(TransitionIdMissing(id))
            )

            /** Like [[updateTransition_]], but a no-op if the given TransitionId is not found
              */
            def updateTransition_(id: TransitionId)(
                f: (Expression, Expression) => T
            ): Invalidating = Kleisli(mapNet =>
                val tids = mapNet.transitionsMap.keySet
                if tids.contains(id)
                then {
                    val oldTransition = mapNet.transitionsMap(id)
                    val newTransition =
                        f(oldTransition.precondition, oldTransition.postcondition)
                    Right(
                      mapNet.copy(transitionsMap = mapNet.transitionsMap.updated(id, newTransition))
                    )
                } else Right(mapNet.invalidate)
            )

            def removeTransition(tid: TransitionId): Invalidating = Kleisli(mapNet =>
                val tids = mapNet.transitionsMap.keySet
                if tids.contains(tid)
                then
                    Right(
                      mapNet.copy(transitionsMap = mapNet.transitionsMap.removed(tid))
                    )
                else Left(TransitionIdMissing(tid))
            )

            def removeTransition_(tid: TransitionId): Invalidating = Kleisli(mapNet =>
                val tids = mapNet.transitionsMap.keySet
                if tids.contains(tid)
                then
                    Right(
                      mapNet.copy(transitionsMap = mapNet.transitionsMap.removed(tid))
                    )
                else Right(mapNet.invalidate)
            )
        }

    }
}
