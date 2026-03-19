package hydrozoa.config.node.operation.multisig

import hydrozoa.lib.number.PositiveInt
import scala.concurrent.duration.{DurationInt, FiniteDuration}

final case class NodeOperationMultisigConfig(
    override val cardanoLiaisonPollingPeriod: FiniteDuration,
    override val peerLiaisonMaxEventsPerBatch: PositiveInt,
    override val blockWeaverTimeout: FiniteDuration
) extends NodeOperationMultisigConfig.Section {
    override transparent inline def nodeOperationMultisigConfig: NodeOperationMultisigConfig = this
}

object NodeOperationMultisigConfig {
    trait Section {
        def nodeOperationMultisigConfig: NodeOperationMultisigConfig

        def cardanoLiaisonPollingPeriod: FiniteDuration
        def peerLiaisonMaxEventsPerBatch: PositiveInt

        /** The length of time that the block weaver will sleep for in the "Leader awaiting" state
          * before waking up to check if the [[TxTiming.majorBlockWakeupTime]] has passed.
          */
        def blockWeaverTimeout: FiniteDuration
    }

    lazy val default = NodeOperationMultisigConfig(
      cardanoLiaisonPollingPeriod = 10.seconds,
      peerLiaisonMaxEventsPerBatch = PositiveInt.unsafeApply(42),
      blockWeaverTimeout = 10.seconds
    )
}
