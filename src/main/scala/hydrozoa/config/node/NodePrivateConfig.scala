package hydrozoa.config.node

import hydrozoa.config.node.operation.evacuation.NodeOperationEvacuationConfig
import hydrozoa.config.node.operation.multisig.NodeOperationMultisigConfig
import hydrozoa.config.node.owninfo.{OwnHeadPeerPrivate, OwnHeadPeerPublic}
import hydrozoa.lib.number.PositiveInt
import hydrozoa.multisig.consensus.peer.{HeadPeerNumber, HeadPeerWallet}
import scala.concurrent.duration.FiniteDuration

final case class NodePrivateConfig(
    override val ownHeadPeerPrivate: OwnHeadPeerPrivate,
    override val nodeOperationEvacuationConfig: NodeOperationEvacuationConfig,
    override val nodeOperationMultisigConfig: NodeOperationMultisigConfig,
) extends NodePrivateConfig.Section {
    override transparent inline def nodePrivateConfig: NodePrivateConfig = this
}

object NodePrivateConfig {
    trait Section
        extends NodeOperationMultisigConfig.Section,
          NodeOperationEvacuationConfig.Section,
          OwnHeadPeerPrivate.Section {
        def nodePrivateConfig: NodePrivateConfig

        override transparent inline def ownHeadWallet: HeadPeerWallet =
            ownHeadPeerPrivate.ownHeadWallet
        override transparent inline def ownHeadPeerPublic: OwnHeadPeerPublic =
            ownHeadPeerPrivate.ownHeadPeerPublic
        override transparent inline def ownHeadPeerNum: HeadPeerNumber =
            ownHeadPeerPrivate.ownHeadPeerNum

        override transparent inline def evacuationBotPollingPeriod: FiniteDuration =
            nodeOperationEvacuationConfig.evacuationBotPollingPeriod

        override transparent inline def cardanoLiaisonPollingPeriod: FiniteDuration =
            nodeOperationMultisigConfig.cardanoLiaisonPollingPeriod

        override transparent inline def peerLiaisonMaxEventsPerBatch: PositiveInt =
            nodeOperationMultisigConfig.peerLiaisonMaxEventsPerBatch

        override transparent inline def evacuationWallet: HeadPeerWallet =
            nodeOperationEvacuationConfig.evacuationWallet
    }
}
