package hydrozoa.config.node

import hydrozoa.config.node.operation.liquidation.NodeOperationLiquidationConfig
import hydrozoa.config.node.operation.multisig.NodeOperationMultisigConfig
import hydrozoa.config.node.owninfo.{OwnHeadPeerPrivate, OwnHeadPeerPublic}
import hydrozoa.lib.number.PositiveInt
import hydrozoa.multisig.consensus.peer.{HeadPeerNumber, HeadPeerWallet}
import scala.concurrent.duration.FiniteDuration

final case class NodePrivateConfig(
    override val ownHeadPeerPrivate: OwnHeadPeerPrivate,
    override val nodeOperationLiquidationConfig: NodeOperationLiquidationConfig,
    override val nodeOperationMultisigConfig: NodeOperationMultisigConfig,
) extends NodePrivateConfig.Section {
    override transparent inline def nodePrivateConfig: NodePrivateConfig = this
}

object NodePrivateConfig {
    trait Section
        extends NodeOperationMultisigConfig.Section,
          NodeOperationLiquidationConfig.Section,
          OwnHeadPeerPrivate.Section {
        def nodePrivateConfig: NodePrivateConfig

        override def ownHeadWallet: HeadPeerWallet =
            ownHeadPeerPrivate.ownHeadWallet
        override def ownHeadPeerPublic: OwnHeadPeerPublic =
            ownHeadPeerPrivate.ownHeadPeerPublic
        override def ownHeadPeerNum: HeadPeerNumber =
            ownHeadPeerPrivate.ownHeadPeerNum

        override def liquidationBotPollingPeriod: FiniteDuration =
            nodeOperationLiquidationConfig.liquidationBotPollingPeriod

        override def cardanoLiaisonPollingPeriod: FiniteDuration =
            nodeOperationMultisigConfig.cardanoLiaisonPollingPeriod

        override def peerLiaisonMaxEventsPerBatch: PositiveInt =
            nodeOperationMultisigConfig.peerLiaisonMaxEventsPerBatch
    }
}
