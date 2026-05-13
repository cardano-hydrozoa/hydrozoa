package hydrozoa.lib.petri

import cats.*
import cats.data.*
import cats.syntax.all.*
import hydrozoa.lib.number.NonNegativeInt
import hydrozoa.lib.petri.net.*
import hydrozoa.lib.petri.net.components.*
import hydrozoa.lib.petri.net.components.Arc.*
import scala.collection.immutable.TreeMap

/** A full, canonical map representation. We use this for building typed nets. TreeMap is selected
  * internally to assist in serialization stability (I guess this isn't strictly needed, but it will
  * make storing json artifacts in version control easier. Should we relax it?).
  *
  * There are many type parameters here. It's a better choice than using path-dependent types
  * bundled in a trait because it makes inference easier. It's recommended not to use this directly
  * because it is verbose; instead, make wrapper types or aliases.
  *
  * MapNet is primarily suitable for _building_ nets rather than simulating. In particular,
  * determining whether a transition is enabled is likely to be much slower than [[MatrixNet]]; the
  * same is true for firing.
  */
case class MapNet[
    NetId,
    ArcId: Ordering,
    PlaceId: Ordering,
    TransitionId: Ordering,
] private (
    override val id: NetId,
    override val name: String,
    override val description: String,
    // Technically there ends up being duplication of IDs. We could reduce this (PlaceWithoutId, etc.), but I don't
    // think its worth it right now
    placesMap: TreeMap[PlaceId, PlaceNoId],
    transitionsMap: TreeMap[TransitionId, TransitionNoId],
    arcsMap: TreeMap[ArcId, ArcNoId[PlaceId, TransitionId]],
) extends Simulator[
      NetId,
      ArcId,
      PlaceId,
      TransitionId,
      MapNet[NetId, ArcId, PlaceId, TransitionId]
    ],
      net.Presentation[ArcId, PlaceId, TransitionId] {

    // Simulator
    extension (a: ArcId) {
        // TODO: This is unsafe. It indicates the need for a simulator _monad_ that can return an error
        def arcPlaceUnsafe: PlaceNoId =
            val pid = arcsMap(a).arcPlaceId
            placesMap(pid)

        // TODO: This is unsafe. It indicates the need for a simulator _monad_ that can return an error
        def arcTransitionUnsafe: TransitionNoId =
            val tid = arcsMap(a).arcTransitionId
            transitionsMap(tid)
    }

    extension (t: TransitionId) {
        def arcs: TreeMap[ArcId, ArcNoId[PlaceId, TransitionId]] =
            arcsMap.filter((id, a) => a.arcTransitionId == id)
    }

    // NOTE: For certain restricted types of nets, this may be less efficient than a matrix/tensor representation
    override def isEnabled(t: TransitionId): Boolean = {
        t.arcs.forall((aid, a) =>
            val p = aid.arcPlaceUnsafe
            a.semantics.necessaryEnablingPredicate(p)
        )
    }

    override protected def fireTransitionUnsafe(
        t: TransitionId
    ): MapNet[NetId, ArcId, PlaceId, TransitionId] = {
        // Recall that two arcs can exist between the same nodes. Thus we need to accumulate a COLLECTION of firing
        // endomorphisms. This is where https://github.com/chimenkamp/YAPNE-Yet-Another-Petri-Net-Editor/issues/13
        // comes in -- we have to decide on the semantics and whether they are sound. I'll do my best for now.
        val placesAndFiringsMap
            : TreeMap[PlaceId, (PlaceNoId, NonEmptyList[Endo[Place.Simulation]])] =
            t.arcs
                .foldRight(
                  TreeMap.empty[
                    PlaceId,
                    (PlaceNoId, NonEmptyList[Endo[Place.Simulation]])
                  ]
                ) { case ((arcId, arc), acc) =>
                    acc.updatedWith(arc.arcPlaceId) {
                        case None =>
                            Some(
                              arcId.arcPlaceUnsafe,
                              NonEmptyList.one(arc.semantics.firingEndomorphism)
                            )
                        case Some(place, firingEndos) =>
                            Some(place, firingEndos.prependK(arc.semantics.firingEndomorphism))
                    }
                }

        // This is where things get tricky -- we've appended a LIST of endomorphisms, but we don't know a priori
        // if they commute.
        // We can solve this either by:
        //   - Fixing an order (right now the order is fixed implicitly, since we construct the list of firing endos
        //     in the same order as the arcs TreeMap -- which is sorted on ArcId ordering.
        //   - Having a valdiation step the ensures the given endos pairwise commute under composition.
        //     This would be hard to prove without dependent types in general, but we could property test during
        //     validation.
        //   - Semi-manually restricting the types of arcs that can have multiplicity
        // I'm ignoring this issue for now.

        val placesAndComposedEndo: TreeMap[PlaceId, (PlaceNoId, Endo[Place.Simulation])] = {
            def foldEndos(endos: NonEmptyList[Endo[Place.Simulation]]): Endo[Place.Simulation] =
                endos.foldLeft(identity: Endo[Place.Simulation])((acc, endo) => acc.andThen(endo))

            placesAndFiringsMap.map { case (pid, (place, endos)) =>
                (pid, (place, foldEndos(endos)))
            }
        }

        val newPlaces: TreeMap[PlaceId, PlaceNoId] =
            placesAndComposedEndo.map { case (pid, (place, endo)) =>
                (pid, place.setTokensUnsafe(place.tokens))
            }

        this.copy(placesMap = this.placesMap ++ newPlaces)
    }

    // ---------------------------------------------------------------------------
    // net.Id
    // ---------------------------------------------------------------------------

    override val places: Set[PlaceId] =
        placesMap.keySet

    override val transitions: Set[TransitionId] =
        transitionsMap.keySet

    override val arcs: Set[ArcId] =
        arcsMap.keySet

    // ---------------------------------------------------------------------------
    // net.Topology
    // ---------------------------------------------------------------------------

    override def getArcTopology(
        arcId: ArcId
    ): Option[components.Arc.Topology[PlaceId, TransitionId]] = arcsMap.get(arcId)

    override def getPlaceTopology(
        placeId: PlaceId
    ): Option[Place.Topology] = placesMap.get(placeId)

    override def getTransitionTopology(
        transitionId: TransitionId
    ): Option[Transition.Topology] = transitionsMap.get(transitionId)

    // ---------------------------------------------------------------------------
    // net.Configuration
    // ---------------------------------------------------------------------------

    override def getArcConfiguration(
        id: ArcId
    ): Option[hydrozoa.lib.petri.net.components.Arc.Semantics] = arcsMap.get(id)

    override def getPlaceConfiguration(
        id: PlaceId
    ): Option[hydrozoa.lib.petri.net.components.Place.Semantics] = placesMap.get(id)

    override def getTransitionConfigurations(
        id: TransitionId
    ): Option[
      hydrozoa.lib.petri.net.components.Transition.Semantics
    ] = transitionsMap.get(id)

    // ---------------------------------------------------------------------------
    // net.Simulation
    // ---------------------------------------------------------------------------

    override def getArcSimulation(
        arcId: ArcId
    ): Option[hydrozoa.lib.petri.net.components.Arc.Simulation] = arcsMap.get(arcId)

    override def getPlaceSimulation(
        placeId: PlaceId
    ): Option[hydrozoa.lib.petri.net.components.Place.Simulation] =
        placesMap.get(placeId)

    override def getTransitionSimulations(
        transitionId: TransitionId
    ): Option[
      hydrozoa.lib.petri.net.components.Transition.Simulation
    ] = transitionsMap.get(transitionId)

    // ---------------------------------------------------------------------------
    // net.Presentation
    // ---------------------------------------------------------------------------

    override def arcPresentations(
        id: ArcId
    ): Option[
      hydrozoa.lib.petri.net.components.Arc.Presentation
    ] = arcsMap.get(id)

    override def getPlacePresentations(
        id: PlaceId
    ): Option[
      hydrozoa.lib.petri.net.components.Place.Presentation
    ] = placesMap.get(id)

    override def transitionPresentations(
        id: TransitionId
    ): Option[
      hydrozoa.lib.petri.net.components.Transition.Presentation
    ] = transitionsMap.get(id)

    def invalidate: MapNet[
      NetId,
      ArcId,
      PlaceId,
      TransitionId,
    ] =
        MapNet(
          id,
          name,
          description,
          placesMap,
          transitionsMap,
          arcsMap
        )
}

object MapNet {
    enum ValidationFlag:
        case Valid
        case Invalid

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
        ArcId: Ordering,
        PlaceId: Ordering,
        TransitionId: Ordering,
    ](
        invalid: MapNet[
          NetId,
          ArcId,
          PlaceId,
          TransitionId
        ]
    ) {

        /** Check for:
          *   - Dangling Arcs
          *   - TODO: Syntactically correct transition pre/post conditions
          */
        def validate: Either[
          NonEmptyList[ValidationError[ArcId, PlaceId, TransitionId]],
          MapNet[
            NetId,
            ArcId,
            PlaceId,
            TransitionId,
          ]
        ] = {
            val arcIdsNormalized: List[(ArcId, PlaceId, TransitionId)] =
                invalid.arcsMap
                    .map((arcId, arc) => (arcId, arc.arcPlaceId, arc.arcTransitionId))
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
                    ArcId,
                    PlaceId,
                    TransitionId,
                  ](
                    id = invalid.id,
                    placesMap = invalid.placesMap,
                    transitionsMap = invalid.transitionsMap,
                    arcsMap = invalid.arcsMap,
                    name = invalid.name,
                    description = invalid.description
                  )
                )
            else Left(NonEmptyList.fromListUnsafe(errors))
        }
    }

    // Empty MapNets are trivially valid, but we still allow for creating invalid ones because
    // sometimes its helpful.
    def empty[
        NetId,
        PlaceId: Ordering,
        TransitionId: Ordering,
        ArcId: Ordering,
    ](id: NetId, name: String, description: String = ""): MapNet[
      NetId,
      PlaceId,
      TransitionId,
      ArcId,
    ] =
        new MapNet(
          id = id,
          placesMap = TreeMap.empty,
          transitionsMap = TreeMap.empty,
          arcsMap = TreeMap.empty,
          name = name,
          description = description
        )

    enum BuilderError[ArcId, PlaceId, TransitionId]:
        case PlaceIdConflict(placeId: PlaceId)
        case PlaceIdMissing(placeId: PlaceId)
        case TransitionIdConflict(transitionId: TransitionId)
        case TransitionIdMissing(transitionId: TransitionId)
        case ArcIdConflict(arcId: ArcId)
        case ArcIdMissing(arcId: ArcId)
    //        case DataVariableIdConflict(dataVariableId: DataVariableId)
    //        case DataVariableIdMissing(dataVariableId: DataVariableId)

    /** Builder monad for typed nets.
      *
      * NOTE: Using F-Bounded polymorphism (https://www.youtube.com/watch?v=Wki2B6iW1oA), we can
      * make this work over arbitrary representations.
      */
    case class BuilderM[
        NetId,
        ArcId: Ordering,
        PlaceId: Ordering,
        TransitionId: Ordering,
        A
    ] private[MapNet] (
        private val builder: IndexedStateT[
          [X] =>> Either[BuilderError[ArcId, PlaceId, TransitionId], X],
          MapNet[
            NetId,
            ArcId,
            PlaceId,
            TransitionId,
          ],
          MapNet[
            NetId,
            ArcId,
            PlaceId,
            TransitionId,
          ],
          A
        ]
    ) {
        def build(netId: NetId, description: String): Either[
          BuilderError[ArcId, PlaceId, TransitionId],
          (
              MapNet[
                NetId,
                ArcId,
                PlaceId,
                TransitionId,
              ],
              A
          )
        ] = builder.run(
          MapNet.empty[
            NetId,
            ArcId,
            PlaceId,
            TransitionId,
          ](netId, description)
        )

        def map[B](
            f: A => B
        ): BuilderM[
          NetId,
          ArcId,
          PlaceId,
          TransitionId,
          B
        ] =
            BuilderM(builder.transform { case (s, a) => (s, f(a)) })

        def flatMap[B](
            fas: A => BuilderM[
              NetId,
              ArcId,
              PlaceId,
              TransitionId,
              B
            ]
        ): BuilderM[
          NetId,
          ArcId,
          PlaceId,
          TransitionId,
          B
        ] =
            BuilderM(
              for {
                  a <- builder
                  b <- fas(a).builder
              } yield b
            )
    }

    object BuilderM {
        given builderMMonad[NetId, ArcId: Ordering, PlaceId: Ordering, TransitionId: Ordering]
            : Monad[[X] =>> BuilderM[NetId, ArcId, PlaceId, TransitionId, X]] with {
            override def flatMap[A, B](fa: BuilderM[NetId, ArcId, PlaceId, TransitionId, A])(
                f: A => BuilderM[NetId, ArcId, PlaceId, TransitionId, B]
            ): BuilderM[NetId, ArcId, PlaceId, TransitionId, B] = fa.flatMap(f)

            override def pure[A](x: A): BuilderM[NetId, ArcId, PlaceId, TransitionId, A] =
                BuilderM(IndexedStateT.pure(x))

            override def tailRecM[A, B](a: A)(
                f: A => BuilderM[NetId, ArcId, PlaceId, TransitionId, Either[A, B]]
            ): BuilderM[NetId, ArcId, PlaceId, TransitionId, B] = BuilderM(
              Monad[[Y] =>> IndexedStateT[
                [X] =>> Either[BuilderError[ArcId, PlaceId, TransitionId], X],
                MapNet[
                  NetId,
                  ArcId,
                  PlaceId,
                  TransitionId,
                ],
                MapNet[
                  NetId,
                  ArcId,
                  PlaceId,
                  TransitionId,
                ],
                Y
              ]].tailRecM(a)(a0 => f(a0).builder)
            )
        }
    }

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
        ArcId: Ordering,
        PlaceId: Ordering,
        TransitionId: Ordering
    ]() {

        import hydrozoa.lib.petri.MapNet.BuilderError.*

        private type MN =
            MapNet[NetId, ArcId, PlaceId, TransitionId]

        private type BM[A] = BuilderM[
          NetId,
          ArcId,
          PlaceId,
          TransitionId,
          A
        ]
        private type P = PlaceNoId
        private type T = TransitionNoId
        private type A = ArcNoId[PlaceId, TransitionId]
        //        private type DV = DataVariable[DataVariableId]

        private type BE = BuilderError[ArcId, PlaceId, TransitionId]

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
            def createPlace(
                pid: PlaceId,
                place: P
            ): BM[Unit] = BuilderM(
              IndexedStateT(mapNet =>
                  val placeIds = mapNet.placesMap.keySet
                  if placeIds.contains(pid)
                  then Left(PlaceIdConflict(pid))
                  else
                      Right(
                        (mapNet.copy(placesMap = mapNet.placesMap.updated(pid, place)), ())
                      )
              )
            )

            /** Like [[createPlace]], but will override existing place IDs.
              */
            def createPlace_(
                pid: PlaceId,
                place: P
            ): BM[Unit] = BuilderM(
              IndexedStateT(mapNet =>
                  Right((mapNet.copy(placesMap = mapNet.placesMap.updated(pid, place)), ()))
              )
            )

            /** Create a transition. Note that if a transition with the given ID already exists,
              * this will return a Left. See also [[createTransition_]]
              */
            def createTransition(
                tid: TransitionId,
                transition: T
            ): BM[Unit] = BuilderM(IndexedStateT(mapNet =>
                val transitionIds = mapNet.transitionsMap.keySet
                if transitionIds.contains(tid)
                then Left(TransitionIdConflict(tid))
                else
                    Right(
                      (
                        mapNet.copy(transitionsMap =
                            mapNet.transitionsMap.updated(tid, transition)
                        ),
                        ()
                      )
                    )
            ))

            /** Like [[createTransition]], but will override existing transition IDs.
              */
            def createTransition_(
                tid: TransitionId,
                transition: T
            ): BM[Unit] = BuilderM(
              IndexedStateT(mapNet =>
                  Right(
                    mapNet.copy(transitionsMap = mapNet.transitionsMap.updated(tid, transition)),
                    ()
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
            private type Invalidating = BM[Unit]

            /** Why it's unsafe: we don't check whether the given source/target ID exists. Will
              * through an error if the given ArcId already exists. See also [[createArc_]]
              */
            def createArc(arcId: ArcId, arc: A): Invalidating = BuilderM(IndexedStateT(mapNet =>
                val arcIds = mapNet.arcsMap.keySet
                if arcIds.contains(arcId)
                then Left(ArcIdConflict(arcId))
                else Right(mapNet.copy(arcsMap = mapNet.arcsMap.updated(arcId, arc)), ())
            ))

            def createArc_(arcId: ArcId, arc: A): Invalidating = BuilderM(
              IndexedStateT(mapNet =>
                  Right(mapNet.copy(arcsMap = mapNet.arcsMap.updated(arcId, arc)), ())
              )
            )

            def updateArc(id: ArcId)(
                f: A => A
            ): Invalidating =
                BuilderM(IndexedStateT(mapNet => {
                    val aids = mapNet.arcsMap.keySet
                    if aids.contains(id)
                    then {
                        val oldArc = mapNet.arcsMap(id)
                        Right(mapNet.copy(arcsMap = mapNet.arcsMap.updated(id, f(oldArc))), ())
                    } else Left(ArcIdMissing(id))
                }))

            def updateArc_(id: ArcId)(
                f: A => A
            ): Invalidating =
                BuilderM(IndexedStateT(mapNet => {
                    val aids = mapNet.arcsMap.keySet
                    if aids.contains(id)
                    then {
                        val oldArc = mapNet.arcsMap(id)
                        Right(mapNet.copy(arcsMap = mapNet.arcsMap.updated(id, f(oldArc))), ())
                    } else Right(mapNet.invalidate, ())
                }))

            def removeArc(aid: ArcId): Invalidating = BuilderM(IndexedStateT(mapNet =>
                val aids = mapNet.arcsMap.keySet
                if aids.contains(aid)
                then Right(mapNet.copy(arcsMap = mapNet.arcsMap.removed(aid)), ())
                else Left(ArcIdMissing(aid))
            ))

            def removeArc_(aid: ArcId): Invalidating = BuilderM(IndexedStateT(mapNet =>
                val aids = mapNet.arcsMap.keySet
                if aids.contains(aid)
                then Right(mapNet.copy(arcsMap = mapNet.arcsMap.removed(aid)), ())
                else Right(mapNet.invalidate, ())
            ))

            /** Update the data associated with the given place ID. If the ID is changed, this may
              * lead to dangling arcs. Will return an error if the given placeId is not found.
              */
            def updatePlace(id: PlaceId)(
                f: (NonNegativeInt, Place.PlaceCapacity, Option[NonNegativeInt]) => P
            ): Invalidating = BuilderM(IndexedStateT(mapNet =>
                val pids = mapNet.placesMap.keySet
                if pids.contains(id)
                then {
                    val oldPlace = mapNet.placesMap(id)
                    val newPlace =
                        f(oldPlace.tokens, oldPlace.capacity, oldPlace.finalMarking)
                    Right(mapNet.copy(placesMap = mapNet.placesMap.updated(id, newPlace)), ())
                } else Left(PlaceIdMissing(id))
            ))

            /** Like [[updatePlace_]], but is a no-op if the given placeId is not found.
              */
            def updatePlace_(id: PlaceId)(
                f: P => P
            ): Invalidating = BuilderM(IndexedStateT(mapNet =>
                val pids = mapNet.placesMap.keySet
                if pids.contains(id)
                then {
                    val newPlace = f(mapNet.placesMap(id))
                    Right(mapNet.copy(placesMap = mapNet.placesMap.updated(id, newPlace)), ())
                } else Right(mapNet.invalidate, ())
            ))

            def removePlace(pid: PlaceId): Invalidating = BuilderM(IndexedStateT(mapNet =>
                val pids = mapNet.placesMap.keySet
                if pids.contains(pid)
                then Right(mapNet.copy(placesMap = mapNet.placesMap.removed(pid)), ())
                else Left(PlaceIdMissing(pid))
            ))

            def removePlace_(pid: PlaceId): Invalidating = BuilderM(IndexedStateT(mapNet =>
                val pids = mapNet.placesMap.keySet
                if pids.contains(pid)
                then Right(mapNet.copy(placesMap = mapNet.placesMap.removed(pid)), ())
                else Right(mapNet.invalidate, ())
            ))

            /** Update the data associated with the given TransitionID. If the ID is changed, this
              * may lead to dangling arcs. Will return an error if the given TransitionId is not
              * found.
              */
            def updateTransition(id: TransitionId)(
                f: T => T
            ): Invalidating = BuilderM(IndexedStateT(mapNet =>
                val tids = mapNet.transitionsMap.keySet
                if tids.contains(id)
                then {
                    val newTransition =
                        f(mapNet.transitionsMap(id))
                    Right(
                      mapNet.copy(transitionsMap =
                          mapNet.transitionsMap.updated(id, newTransition)
                      ),
                      ()
                    )
                } else Left(TransitionIdMissing(id))
            ))

            /** Like [[updateTransition_]], but a no-op if the given TransitionId is not found
              */
            def updateTransition_(id: TransitionId)(
                f: T => T
            ): Invalidating = BuilderM(IndexedStateT(mapNet =>
                val tids = mapNet.transitionsMap.keySet
                if tids.contains(id)
                then {
                    val newTransition = f(mapNet.transitionsMap(id))
                    Right(
                      mapNet.copy(transitionsMap =
                          mapNet.transitionsMap.updated(id, newTransition)
                      ),
                      ()
                    )
                } else Right(mapNet.invalidate, ())
            ))

            def removeTransition(tid: TransitionId): Invalidating = BuilderM(IndexedStateT(mapNet =>
                val tids = mapNet.transitionsMap.keySet
                if tids.contains(tid)
                then
                    Right(
                      mapNet.copy(transitionsMap = mapNet.transitionsMap.removed(tid)),
                      ()
                    )
                else Left(TransitionIdMissing(tid))
            ))

            def removeTransition_(tid: TransitionId): Invalidating = BuilderM(
              IndexedStateT(mapNet =>
                  val tids = mapNet.transitionsMap.keySet
                  if tids.contains(tid)
                  then Right(mapNet.copy(transitionsMap = mapNet.transitionsMap.removed(tid)), ())
                  else Right(mapNet.invalidate, ())
              )
            )
        }
    }
}
