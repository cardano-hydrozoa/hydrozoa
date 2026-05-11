package hydrozoa.lib.petri

import hydrozoa.lib.number.PositiveInt
import hydrozoa.lib.petri.Place.PlacePresentation
import hydrozoa.lib.petri.Transition.TransitionPresentation
import scala.annotation.tailrec
import scala.collection.immutable.TreeSet
import scala.util.Random

object Net {

    /** A presentation associated with a valid net [[Simulation]]. This tells us the position,
      * labels, and size of the elements of the net.
      *
      * Note that a presentation can be valid or invalid independently of the validity of the
      * underlying TopologyAndMarking. This is because the given ID <> Presentation mappings can be
      * added out of order during serialization.
      */
    trait Presentation[
        NetId,
        PlaceId,
        TransitionId,
        ArcId,
        DataVariableId,
        ArcLabel,
        PlaceLabel,
        TransitionLabel,
        PresentationVF <: ValidationFlag
    ] {
        // TODO: Maybe these should be polymorphic. It will matter when we start combining subnets into larger nets
        def name: String
        def description: String

        def net: Simulation[
          NetId,
          PlaceId,
          TransitionId,
          ArcId,
          DataVariableId,
        ]

        def arcPresentations: Map[ArcId, ArcPresentation[ArcLabel]]
        def placePresentations: Map[PlaceId, PlacePresentation[PlaceLabel]]
        def transitionPresentations: Map[TransitionId, TransitionPresentation[TransitionLabel]]
        def dataVariablePresentations: Map[DataVariableId, DataVariablePresentation]

    }

    /** Used as a phantom type to indicate whether a net representation has passed validation
      */
    enum ValidationFlag:
        case Valid
        case Invalid

    /** Different representations of the net serve different purposes, but [[Valid]] nets MUST be
      * isomorphic. For example, the MapNet is a simple representation for building a net, while
      * matrix representations are more suitable for fast simulation and analysis
      */
    // TODO: YAPNE (at least in its serialization) combines the topology of the net with the marking.
    // I feel that these should be separated, such that the places should not carry tokens and the
    // data variables should not carry actual values (just types). But this is easier for now, so I'll stick with it
    // until I have more experience to inform the future layout
    trait TopologyAndMarking[
        NetId,
        PlaceId,
        TransitionId,
        ArcId,
        DataVariableId,
        VF <: ValidationFlag
    ] {
        val id: NetId
        lazy val places: Seq[Place[PlaceId]]
        lazy val transitions: Seq[Transition[TransitionId]]
        lazy val arcs: Seq[Arc[ArcId, TransitionId, PlaceId]]
        lazy val dataVariables: Seq[DataVariable[DataVariableId]]
    }

    object Simulation {
        enum Error[TransitionId]:
            case TransitionNotEnabled(id: TransitionId)
    }

    /** A "Simulation" is only possible on a topologically-valid net.
      */
    trait Simulation[NetId, PlaceId, TransitionId, ArcId, DataVariableId]
        extends TopologyAndMarking[
          NetId,
          PlaceId,
          TransitionId,
          ArcId,
          DataVariableId,
          ValidationFlag.Valid.type
        ] {
        private type Sim = Simulation[NetId, PlaceId, TransitionId, ArcId, DataVariableId]

        lazy val getEnabledTransitions: TreeSet[TransitionId]
        protected def fireTransitionUnsafe(id: TransitionId): Sim

        final def fireTransition(id: TransitionId): Either[Simulation.Error[TransitionId], Sim] =
            if getEnabledTransitions.contains(id)
            then Right(fireTransitionUnsafe(id))
            else Left(Simulation.Error.TransitionNotEnabled(id))

        // TODO:
        //   - This currently serializes transitions. Make it pick from a set of non-conflicting transitions
        //     and execute them simultaneously
        //   - Return the execution trace log
        //   - This does not check if the net has cycles. If it does, it will run forever.
        @tailrec
        final def autoFireEnabledTransitions(
            maxSteps: Option[PositiveInt],
            random: Random = Random(0)
        ): Sim =
            if getEnabledTransitions.isEmpty
            then this
            else {
                val index = random.nextInt(getEnabledTransitions.size)
                val t =
                    getEnabledTransitions.toIndexedSeq(index)
                val fired = fireTransitionUnsafe(t)
                maxSteps match {
                    case None =>
                        fired.autoFireEnabledTransitions(None, random)
                    case Some(p) if p.convert == 1 => fired
                    case Some(p) =>
                        fired.autoFireEnabledTransitions(
                          Some(PositiveInt.unsafeApply(p.convert - 1)),
                          random
                        )

                }
            }

    }
}
