package hydrozoa.timingviz

import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.timingviz.Ids.ObjectId

/** Reasons a `Command` did not produce a state change. Distinct from `UIHint` because rejections
  * mean nothing landed; hints are advisory.
  */
enum Rejection:
    case DuplicateObject(id: ObjectId)
    case UnknownObject(id: ObjectId)
    case InvalidInterval(start: QuantizedInstant, end: QuantizedInstant)
    case ClockWentBackwards(from: QuantizedInstant, to: QuantizedInstant)
    case ParameterViolation(reason: String)

/** Advisory feedback the renderer surfaces alongside a successful transition: snap-to-grid
  * adjustments, derivation edges to highlight, warnings about timing-rule margins.
  */
enum UIHint:
    case Snapped(field: FieldKey, from: QuantizedInstant, to: QuantizedInstant)
    case DerivationAdded(target: (ObjectId, FieldKey), sources: Set[(ObjectId, FieldKey)])
    case MarginWarning(message: String)
