package hydrozoa.config.node

import hydrozoa.config.head.coil.CoilPeers
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.config.node.operation.evacuation.NodeOperationEvacuationConfig
import hydrozoa.config.node.operation.multisig.NodeOperationMultisigConfig
import hydrozoa.config.node.owninfo.OwnPeerPrivate
import io.circe.*
import io.circe.generic.semiauto.*

final case class NodePrivateConfig(
    override val ownPeerPrivate: OwnPeerPrivate,
    override val nodeOperationEvacuationConfig: NodeOperationEvacuationConfig,
    override val nodeOperationMultisigConfig: NodeOperationMultisigConfig,
    override val blockfrostApiKey: String,
    override val remoteLedgerUri: Option[String],
    override val remoteScreenerUri: Option[String] = None,
    override val adminUsername: String,
    override val adminPassword: String,
    override val httpHost: String,
    override val httpPort: String,
    override val peerBindHost: Option[String] = None,
    override val peerBindPort: Option[String] = None,
) extends NodePrivateConfig.Section {
    override transparent inline def nodePrivateConfig: NodePrivateConfig = this
}

object NodePrivateConfig {
    trait Section
        extends NodeOperationMultisigConfig.Section,
          NodeOperationEvacuationConfig.Section,
          OwnPeerPrivate.Section {
        def nodePrivateConfig: NodePrivateConfig

        def ownPeerPrivate: OwnPeerPrivate = nodePrivateConfig.ownPeerPrivate

        def nodeOperationEvacuationConfig: NodeOperationEvacuationConfig =
            nodePrivateConfig.nodeOperationEvacuationConfig

        def nodeOperationMultisigConfig: NodeOperationMultisigConfig =
            nodePrivateConfig.nodeOperationMultisigConfig

        def blockfrostApiKey: String = nodePrivateConfig.blockfrostApiKey

        /** WS URI of the remote L2 ledger, required only when `l2Ledger = any-remote`; a
          * `cardano-eutxo` node runs its ledger in-process and omits it.
          */
        def remoteLedgerUri: Option[String] = nodePrivateConfig.remoteLedgerUri

        /** HTTP URI of the remote ledger's screening endpoint (`l2Ledger = any-remote` only).
          * Optional even then: a node without it screens nothing pre-RequestId (the passthrough
          * behaviour every remote node had before the endpoint existed).
          */
        def remoteScreenerUri: Option[String] = nodePrivateConfig.remoteScreenerUri

        def adminUsername: String = nodePrivateConfig.adminUsername

        def adminPassword: String = nodePrivateConfig.adminPassword

        def httpHost: String = nodePrivateConfig.httpHost

        def httpPort: String = nodePrivateConfig.httpPort

        /** Where this node's peer websocket server actually binds, when that is not where other
          * peers reach it.
          *
          * A peer's `webSocketAddress` in the shared head config is the address the rest of the
          * head dials, so every peer must agree on it. It is also, by default, the address this
          * node binds. Those coincide on a flat network and stop coinciding the moment anything
          * sits in front: behind a TLS-terminating proxy the head is dialed at
          * `wss://head.example:4001` but must listen on a private interface the proxy can reach,
          * and `head.example` is not an address this host can bind at all.
          *
          * Being per-node private config is the point: the bind address is this machine's business,
          * and putting it in the shared config would force every peer to agree on a detail none of
          * them can observe.
          */
        def peerBindHost: Option[String] = nodePrivateConfig.peerBindHost

        /** Port counterpart to [[peerBindHost]]; see there. Settable independently, so a node can
          * keep the advertised port and move only the local one.
          */
        def peerBindPort: Option[String] = nodePrivateConfig.peerBindPort
    }

    given nodePrivateConfigEncoder: Encoder[NodePrivateConfig] =
        deriveEncoder[NodePrivateConfig]

    given nodePrivateConfigDecoder(using
        headPeers: HeadPeers.Section,
        coilPeers: CoilPeers
    ): Decoder[NodePrivateConfig] = deriveDecoder[NodePrivateConfig]
}
