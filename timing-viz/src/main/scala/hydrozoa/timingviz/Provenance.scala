package hydrozoa.timingviz

import hydrozoa.timingviz.Ids.ObjectId

/** Names a single time-valued field on a `TimedObject`. Used as the key (or value) of an entry in
  * the derivation graph that records "field F of object X was computed from field G of object Y."
  */
enum FieldKey:
    case ValidityStart, ValidityEnd
    case CreationStart, CreationEnd
    case FallbackStart
    case RefundStart
    case AbsorptionStart, AbsorptionEnd
    case SubmissionDeadline
    case ForcedMajorWakeup

/** "Field F of object X derives from these (object, field) pairs." Powers hover-highlight ("why is
  * this here?") and the draft-mode cascade ("dragging this knob shifts these intervals").
  */
type DerivationMap = Map[(ObjectId, FieldKey), Set[(ObjectId, FieldKey)]]
