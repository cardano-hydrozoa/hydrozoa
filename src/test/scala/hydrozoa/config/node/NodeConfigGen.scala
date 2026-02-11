package hydrozoa.config.node

import hydrozoa.config.head.{HeadConfig, HeadPeersSpec}
import hydrozoa.config.node.operation.liquidation.NodeOperationLiquidationConfig
import hydrozoa.config.node.operation.multisig.NodeOperationMultisigConfig
import hydrozoa.config.node.owninfo.OwnHeadPeerPrivate
import org.scalacheck.{Gen, Prop, Properties}

// We pass in the exact head peers so that the head config and own private peer are parameterized on the same
// peer set
def generateNodeConfig(headPeersSpec: HeadPeersSpec.Exact)(
    generateHeadConfig: Gen[HeadConfig] = hydrozoa.config.head.generateHeadConfig(headPeersSpec)(),
    generateOwnHeadPeerPrivate: Gen[OwnHeadPeerPrivate] =
        hydrozoa.config.node.owninfo.generateOwnHeadPeerPrivate(headPeersSpec),
    generateNodeOperationLiquidationConfig: Gen[NodeOperationLiquidationConfig] =
        hydrozoa.config.node.operation.liquidation.generateNodeOperationLiquidationConfig,
    generateNodeOperationMultisigConfig: Gen[NodeOperationMultisigConfig] =
        hydrozoa.config.node.operation.multisig.generateNodeOperationMultisigConfig
): Gen[NodeConfig] =
    for {
        headConfig <- generateHeadConfig
        ownHeadWallet <- generateOwnHeadPeerPrivate.map(_.ownHeadWallet)
        nodeOperationLiquidationConfig <- generateNodeOperationLiquidationConfig
        nodeOperationMultisigConfig <- generateNodeOperationMultisigConfig
    } yield NodeConfig(
      headConfig = headConfig,
      ownHeadWallet = ownHeadWallet,
      nodeOperationLiquidationConfig = nodeOperationLiquidationConfig,
      nodeOperationMultisigConfig = nodeOperationMultisigConfig
    ).get

object NodeConfigGen extends Properties("Sanity Check") {
    val _ = property("sanity check") =
        Prop.forAll(generateNodeConfig(HeadPeersSpec.Exact(3))())(_ => true)
}
