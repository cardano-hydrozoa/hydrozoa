package hydrozoa.timingviz

/** Where a `TimedObject` came from. The renderer uses this to distinguish observed reality (solid)
  * from user-issued hypotheticals (dashed / faded).
  */
enum Source:
    case Observed
    case Hypothetical

/** A `TimedObject` together with its `Source`. Object identity (`objectId`) is delegated to the
  * wrapped object, so the map keying is unchanged.
  */
final case class TaggedObject(timedObject: TimedObject, source: Source)
