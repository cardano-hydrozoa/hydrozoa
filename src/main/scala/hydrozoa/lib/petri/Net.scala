package hydrozoa.lib.petri

object Net {
//
//
//
//    /** A presentation associated with a valid net [[Simulation]]. This tells us the position,
//      * labels, and size of the elements of the net.
//      *
//      * Note that a presentation can be valid or invalid independently of the validity of the
//      * underlying TopologyAndMarking. This is because the given ID <> Presentation mappings can be
//      * added out of order during serialization.
//      */
//    trait Presentation[
//        NetId,
//        PlaceId,
//        TransitionId,
//        ArcId,
//        DataVariableId,
//        ArcLabel,
//        PlaceLabel,
//        TransitionLabel,
//        PresentationVF <: ValidationFlag
//    ] {
//        // TODO: Maybe these should be polymorphic. It will matter when we start combining subnets into larger nets
//        def name: String
//        def description: String
//
//        def net: Simulation[
//          NetId,
//          PlaceId,
//          TransitionId,
//          ArcId,
//          DataVariableId,
//        ]
//
//        def arcPresentations: Map[ArcId, ArcPresentation[ArcLabel]]
//        def placePresentations: Map[PlaceId, PlacePresentation[PlaceLabel]]
//        def transitionPresentations: Map[TransitionId, TransitionPresentation[TransitionLabel]]
//        def dataVariablePresentations: Map[DataVariableId, DataVariablePresentation]
//
//    }
//
//    /** Used as a phantom type to indicate whether a net representation has passed validation
//      */
//    enum ValidationFlag:
//        case Valid
//        case Invalid
//
//
//

//
//    /** A "Simulation" is only possible on a topologically-valid net.
//      */
//    trait Simulation[NetId, PlaceId, TransitionId, ArcId, DataVariableId]
//        extends TopologyAndMarking[
//          NetId,
//          PlaceId,
//          TransitionId,
//          ArcId,
//          DataVariableId,
//          ValidationFlag.Valid.type
//        ] {
//        private type Sim = Simulation[NetId, PlaceId, TransitionId, ArcId, DataVariableId]
//

}
