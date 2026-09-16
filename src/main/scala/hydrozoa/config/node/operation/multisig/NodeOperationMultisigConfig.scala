package hydrozoa.config.node.operation.multisig

import hydrozoa.lib.cardano.scalus.QuantizedTime.given
import hydrozoa.lib.number.PositiveInt
import io.circe.*
import io.circe.generic.semiauto.*
import scala.concurrent.duration.{DurationInt, FiniteDuration}

final case class NodeOperationMultisigConfig(
    override val cardanoLiaisonPollingPeriod: FiniteDuration,
    override val peerLiaisonMaxRequestsPerBatch: PositiveInt,
    override val peerLiaisonOutboxCap: PositiveInt,
    override val peerLiaisonResendInterval: FiniteDuration,
    override val coilCatchUpStacks: Int,
    override val rateLimits: RateLimits
) extends NodeOperationMultisigConfig.Section {
    override transparent inline def nodeOperationMultisigConfig: NodeOperationMultisigConfig = this
}

object NodeOperationMultisigConfig {
    trait Section extends RateLimits.Section {
        def nodeOperationMultisigConfig: NodeOperationMultisigConfig

        def cardanoLiaisonPollingPeriod: FiniteDuration =
            nodeOperationMultisigConfig.cardanoLiaisonPollingPeriod
        def peerLiaisonMaxRequestsPerBatch: PositiveInt =
            nodeOperationMultisigConfig.peerLiaisonMaxRequestsPerBatch

        /** How many items each [[hydrozoa.multisig.consensus.liaison.LaneOutbound]] keeps in
          * memory, floored at that lane's `maxPerReply`. Above the cap the oldest is evicted; a
          * pull below the remaining floor is served from the journal instead, which is sound
          * because nothing reaches a lane before it is durable (CR4) and is already how every lane
          * serves after a restart.
          *
          * Node-local on purpose: it changes only how much this peer caches, never what it sends,
          * so peers may run different values without diverging.
          */
        def peerLiaisonOutboxCap: PositiveInt =
            nodeOperationMultisigConfig.peerLiaisonOutboxCap

        /** How often each [[hydrozoa.multisig.consensus.PeerLiaisonHeadToHead]] re-sends its
          * currently outstanding `GetMsgBatch` to the remote peer, to recover from a stalled
          * request-response chain (e.g. caused by a dropped WS frame). The re-send is idempotent on
          * the wire.
          */
        def peerLiaisonResendInterval: FiniteDuration =
            nodeOperationMultisigConfig.peerLiaisonResendInterval

        /** How many stacks behind this hub a coil peer may be and still be left to **catch up**
          * over the population lanes, rather than be seeded with a transferred state (GUM-312).
          *
          * A reconnecting coil is essentially always behind — a hub produces continuously — so
          * without a threshold every brief disconnect would trigger a full state transfer. This is
          * the line between "walk forward over the lanes, which is what they are for" and "that is
          * further than replaying is worth".
          *
          * Measured in **stacks**, the unit a start point is named in and the one a coil reports.
          * The cost it stands for is really the blocks and requests replayed underneath, so the two
          * only track each other as closely as stack size is uniform.
          *
          * ⚠️ The default is a placeholder, not a measurement: nobody has yet compared the cost of
          * replaying N stacks against shipping one full utxo-set snapshot. Expect to move it once
          * there are numbers, and note the sensible ceiling is whatever history retention keeps
          * ([[hydrozoa.multisig.persistence.Cf]] pruning) — past that, catching up is impossible
          * however cheap it would have been.
          *
          * Node-local, like [[peerLiaisonOutboxCap]]: it changes only which route this hub takes to
          * get a coil current, never what any peer signs, so hubs may run different values without
          * diverging.
          */
        def coilCatchUpStacks: Int = nodeOperationMultisigConfig.coilCatchUpStacks

        override def rateLimits: RateLimits = nodeOperationMultisigConfig.rateLimits
    }

    /** Two `peerLiaisonMaxRequestsPerBatch` batches of requests, which is also generous headroom on
      * the lanes that reply one item at a time. At the market-maker payload mix a request lane then
      * holds tens of MB rather than everything the process has ever relayed.
      */
    val defaultPeerLiaisonOutboxCap: PositiveInt = PositiveInt.unsafeApply(1024)

    /** Placeholder until the replay-vs-transfer cost is measured -- see `coilCatchUpStacks`. Chosen
      * to make a momentary disconnect cheap (catch up) and a genuinely stale store expensive
      * (seed), without claiming to know where the real crossover sits.
      */
    val defaultCoilCatchUpStacks: Int = 16

    lazy val default: NodeOperationMultisigConfig = NodeOperationMultisigConfig(
      cardanoLiaisonPollingPeriod = 10.seconds,
      peerLiaisonMaxRequestsPerBatch = PositiveInt.unsafeApply(500),
      peerLiaisonOutboxCap = defaultPeerLiaisonOutboxCap,
      peerLiaisonResendInterval = 5.seconds,
      coilCatchUpStacks = defaultCoilCatchUpStacks,
      rateLimits = RateLimits.default
    )

    given Encoder[NodeOperationMultisigConfig] = deriveEncoder[NodeOperationMultisigConfig]

    /** Hand-written rather than derived so `peerLiaisonOutboxCap` may be **absent**: every config
      * file written before this field existed must still decode, and a node whose config fails to
      * decode does not start at all.
      */
    given Decoder[NodeOperationMultisigConfig] = Decoder.instance(c =>
        for {
            pollingPeriod <- c.downField("cardanoLiaisonPollingPeriod").as[FiniteDuration]
            maxRequestsPerBatch <- c.downField("peerLiaisonMaxRequestsPerBatch").as[PositiveInt]
            outboxCap <- c.downField("peerLiaisonOutboxCap").as[Option[PositiveInt]]
            resendInterval <- c.downField("peerLiaisonResendInterval").as[FiniteDuration]
            catchUp <- c.downField("coilCatchUpStacks").as[Option[Int]]
            limits <- c.downField("rateLimits").as[RateLimits]
        } yield NodeOperationMultisigConfig(
          cardanoLiaisonPollingPeriod = pollingPeriod,
          peerLiaisonMaxRequestsPerBatch = maxRequestsPerBatch,
          peerLiaisonOutboxCap = outboxCap.getOrElse(defaultPeerLiaisonOutboxCap),
          peerLiaisonResendInterval = resendInterval,
          coilCatchUpStacks = catchUp.getOrElse(defaultCoilCatchUpStacks),
          rateLimits = limits
        )
    )
}
