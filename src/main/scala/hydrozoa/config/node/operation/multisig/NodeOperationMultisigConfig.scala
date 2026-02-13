package hydrozoa.config.node.operation.multisig

import hydrozoa.lib.number.PositiveInt
import scala.concurrent.duration.FiniteDuration

final case class NodeOperationMultisigConfig(
    override val cardanoLiaisonPollingPeriod: FiniteDuration,
    override val peerLiaisonMaxEventsPerBatch: PositiveInt,
) extends NodeOperationMultisigConfig.Section {
    override transparent inline def nodeOperationMultisigConfig: NodeOperationMultisigConfig = this
}

object NodeOperationMultisigConfig {
    trait Section {
        def nodeOperationMultisigConfig: NodeOperationMultisigConfig

        def cardanoLiaisonPollingPeriod: FiniteDuration
        def peerLiaisonMaxEventsPerBatch: PositiveInt
    }
}
