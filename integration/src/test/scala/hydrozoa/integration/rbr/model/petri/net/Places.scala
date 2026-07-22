package hydrozoa.integration.rbr.model.petri.net

/** The places of the RBR dispute/evacuation flow, used by the property tests to bucket L1 UTxOs
  * into a histogram (see `rbr.property.Abstraction`). The colored HLPN model lives in
  * `rbr.model.petri.hlpn.RBRHlNet`.
  */
enum RBRPlaceId:
    case TreasuryRefPlaceId
    case DisputeRefPlaceId
    case RegimeRefPlaceId
    case SetupLadderRefPlaceId
    case UnresolvedTreasuryPlaceId
    case ResolvedTreasuryPlaceId
    case UnvotedPlaceId
    case VotedPlaceId
    case PayoutObligationsPlaceId // synthetic
    case EvacuationOutputPlaceId
    case CollateralPlaceId
    case AmbientPlaceId

object RBRPlaceId {
    given Ordering[RBRPlaceId] = Ordering.by(_.ordinal)
}
