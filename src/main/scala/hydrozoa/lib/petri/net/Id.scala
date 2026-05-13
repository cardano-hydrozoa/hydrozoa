package hydrozoa.lib.petri.net

/** This the authoritative source of IDs in the net in the following sense:
  *   - If there is some data that MUST be associated every ID of a component (such as topology,
  *     configuration, etc), then it is invalid to NOT have a total mapping from these sets to the
  *     associated data
  *   - If an association between an ID and data exists, then the ID MUST appear in this set for the
  *     association to be considered valid.
  * @tparam NetId
  * @tparam PlaceId
  * @tparam TransitionId
  * @tparam ArcId
  */
trait Id[NetId, ArcId, PlaceId, TransitionId] {
    val id: NetId
    val arcs: Set[ArcId]
    val places: Set[PlaceId]
    val transitions: Set[TransitionId]
    // TODO: Add data variables
}
