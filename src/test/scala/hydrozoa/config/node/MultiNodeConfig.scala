package hydrozoa.config.node

import cats.data.NonEmptyList
import hydrozoa.config.head.initialization.BlockCreationEndTimeGen.currentTimeBlockCreationEndTime
import hydrozoa.config.head.initialization.{InitializationParametersGenBottomUp, InitializationParametersGenTopDown}
import hydrozoa.config.head.multisig.fallback.{FallbackContingencyGen, generateFallbackContingency}
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.BlockCreationEndTime
import hydrozoa.config.head.multisig.timing.{TxTimingGen, generateDefaultTxTiming}
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.parameters.{GenHeadParams, generateHeadParameters}
import hydrozoa.config.head.rulebased.{DisputeResolutionConfigGen, generateDisputeResolutionConfig}
import hydrozoa.config.head.{HeadConfig, HeadConfigGen}
import hydrozoa.config.node.operation.liquidation.{NodeOperationLiquidationConfig, generateNodeOperationLiquidationConfig}
import hydrozoa.config.node.operation.multisig.{NodeOperationMultisigConfig, generateNodeOperationMultisigConfig}
import hydrozoa.config.node.owninfo.OwnHeadPeerPrivate
import hydrozoa.lib.cardano.scalus.ShelleyAddressExtra
import hydrozoa.lib.cardano.scalus.txbuilder.Transaction.attachVKeyWitnesses
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.block.BlockHeader
import org.scalacheck.{Gen, Prop, Properties}
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.{SlotConfig, Transaction, VKeyWitness}
import test.{TestPeers, TestPeersSpec}

/** Multi-node config is a tool for test suites that allows multisigning effects as well ad gives
  * the access to the head config, which is common for all peers.
  *
  * @param nodePrivateConfigs
  * @param headConfig
  */
case class MultiNodeConfig private (
    nodePrivateConfigs: Map[HeadPeerNumber, NodePrivateConfig],
    headConfig: HeadConfig,
) {
    lazy val nodeConfigs: Map[HeadPeerNumber, NodeConfig] =
        nodePrivateConfigs.map((n, pc) =>
            n ->
                NodeConfig(
                  headConfig,
                  pc.ownHeadWallet,
                  pc.nodeOperationLiquidationConfig,
                  pc.nodeOperationMultisigConfig
                ).get
        )

    def multisignTx(tx: Transaction): Transaction =
        tx.attachVKeyWitnesses(mkVKeyWitnesses(tx).toList)

    def mkVKeyWitnesses(tx: Transaction): NonEmptyList[VKeyWitness] =
        NonEmptyList.fromListUnsafe(
          nodePrivateConfigs.map(_._2.ownHeadWallet.mkVKeyWitness(tx)).toList
        )

    def multisignHeader(
        blockHeader: BlockHeader.Minor.Onchain
    ): NonEmptyList[BlockHeader.Minor.HeaderSignature] =
        val serialized = BlockHeader.Minor.Onchain.Serialized(blockHeader)
        NonEmptyList.fromListUnsafe(
          nodePrivateConfigs.map(_._2.ownHeadWallet.mkMinorHeaderSignature(serialized)).toList
        )

    def addressOf(peerNumber: HeadPeerNumber): ShelleyAddress =
        ShelleyAddressExtra.mkShelleyAddress(
          nodeConfigs(peerNumber).ownHeadWallet.exportVerificationKey,
          headConfig.network
        )

    def signTxAs(peerNumber: HeadPeerNumber) = nodeConfigs(peerNumber).ownHeadWallet.signTx

    // TODO: are we fine with having that here? Better place?
    def pickPeer: Gen[HeadPeerNumber] =
        Gen.choose(0, nodePrivateConfigs.size - 1)
            .map(HeadPeerNumber.apply)

}

object MultiNodeConfig {

    def generate(spec: TestPeersSpec)(
        generateHeadConfig: HeadConfigGen = hydrozoa.config.head.generateHeadConfig,
        generateHeadStartTime: SlotConfig => Gen[BlockCreationEndTime] =
            currentTimeBlockCreationEndTime,
        generateTxTiming: TxTimingGen = generateDefaultTxTiming,
        generateFallbackContingency: FallbackContingencyGen = generateFallbackContingency,
        generateDisputeResolutionConfig: DisputeResolutionConfigGen =
            generateDisputeResolutionConfig,
        generateHeadParameters: GenHeadParams = generateHeadParameters,
        generateInitializationParameters: InitializationParametersGenBottomUp.GenInitializationParameters |
            InitializationParametersGenTopDown.GenWithDeps =
            InitializationParametersGenBottomUp.generateInitializationParameters,
        generateNodeOperationLiquidationConfig: Gen[NodeOperationLiquidationConfig] =
            generateNodeOperationLiquidationConfig,
        generateNodeOperationMultisigConfig: Gen[NodeOperationMultisigConfig] =
            generateNodeOperationMultisigConfig
    ): Gen[MultiNodeConfig] = for {
        testPeers <- TestPeers.generate(spec)
        ret <- generateForTestPeers(testPeers)(
          generateHeadConfig,
          generateHeadStartTime,
          generateTxTiming,
          generateFallbackContingency,
          generateDisputeResolutionConfig,
          generateHeadParameters,
          generateInitializationParameters,
          generateNodeOperationLiquidationConfig,
          generateNodeOperationMultisigConfig
        )
    } yield ret

    def generateForTestPeers(testPeers: TestPeers)(
        generateHeadConfig: HeadConfigGen = hydrozoa.config.head.generateHeadConfig,
        generateBlockCreationEndTime: SlotConfig => Gen[BlockCreationEndTime] =
            currentTimeBlockCreationEndTime,
        generateTxTiming: TxTimingGen = generateDefaultTxTiming,
        generateFallbackContingency: FallbackContingencyGen = generateFallbackContingency,
        generateDisputeResolutionConfig: DisputeResolutionConfigGen =
            generateDisputeResolutionConfig,
        generateHeadParameters: GenHeadParams = generateHeadParameters,
        generateInitializationParameters: InitializationParametersGenBottomUp.GenInitializationParameters |
            InitializationParametersGenTopDown.GenWithDeps =
            InitializationParametersGenBottomUp.generateInitializationParameters,
        generateNodeOperationLiquidationConfig: Gen[NodeOperationLiquidationConfig] =
            generateNodeOperationLiquidationConfig,
        generateNodeOperationMultisigConfig: Gen[NodeOperationMultisigConfig] =
            generateNodeOperationMultisigConfig
    ): Gen[MultiNodeConfig] =
        for {
            headConfig <- generateHeadConfig(testPeers)(
              generateBlockCreationEndTime = generateBlockCreationEndTime,
              generateTxTiming = generateTxTiming,
              generateFallbackContingency = generateFallbackContingency,
              generateDisputeResolutionConfig = generateDisputeResolutionConfig,
              generateHeadParameters = generateHeadParameters,
              generateInitializationParameters = generateInitializationParameters
            )

            nodePrivateConfigs <-
                Gen.sequence[List[
                  (HeadPeerNumber, NodePrivateConfig)
                ], (HeadPeerNumber, NodePrivateConfig)](
                  testPeers.headPeerIds.toList.map(peerId =>
                      for {
                          nolc <- generateNodeOperationLiquidationConfig
                          nomc <- generateNodeOperationMultisigConfig
                          ohpp = OwnHeadPeerPrivate(
                            testPeers.walletFor(peerId._1),
                            headConfig.headPeers
                          ).get
                      } yield peerId._1 -> NodePrivateConfig(
                        ownHeadPeerPrivate = ohpp,
                        nodeOperationLiquidationConfig = nolc,
                        nodeOperationMultisigConfig = nomc
                      )
                  )
                )
        } yield new MultiNodeConfig(
          nodePrivateConfigs = nodePrivateConfigs.toMap,
          headConfig = headConfig
        )
}

object MultiNodeConfigTest extends Properties("Multi-node config") {
    val _ = property("generates") = Prop.forAll(
      TestPeersSpec
          .generate()
          .flatMap(MultiNodeConfig.generate(_)())
    )(_ => true)
}
