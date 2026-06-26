package hydrozoa.timingviz

/** Identifiers for the four kinds of things the visualizer tracks. Each is an opaque alias over
  * `String` so the type checker keeps them distinct even though the representation is shared.
  */
object Ids:
    opaque type BlockId = String
    object BlockId:
        def apply(s: String): BlockId = s
        extension (b: BlockId) def asString: String = b

    opaque type RequestId = String
    object RequestId:
        def apply(s: String): RequestId = s
        extension (r: RequestId) def asString: String = r

    opaque type DepositId = String
    object DepositId:
        def apply(s: String): DepositId = s
        extension (d: DepositId) def asString: String = d

    /** Settlement, initialization, finalization, fallback, and refund effects all share one id type
      * because the visualizer treats them uniformly along the timeline.
      */
    opaque type EffectId = String
    object EffectId:
        def apply(s: String): EffectId = s
        extension (e: EffectId) def asString: String = e

    enum ObjectId:
        case OfBlock(id: BlockId)
        case OfRequest(id: RequestId)
        case OfDeposit(id: DepositId)
        case OfEffect(id: EffectId)
