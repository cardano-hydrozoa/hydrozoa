package hydrozoa.config.node

import cats.data.NonEmptyList
import cats.effect.unsafe.IORuntime
import hydrozoa.config.head.initialization.BlockCreationEndTimeGen.currentTimeBlockCreationEndTime
import hydrozoa.config.head.initialization.{InitializationParametersGenBottomUp, InitializationParametersGenTopDown}
import hydrozoa.config.head.multisig.fallback.{FallbackContingencyGen, generateFallbackContingency}
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.BlockCreationEndTime
import hydrozoa.config.head.multisig.timing.{TxTimingGen, generateDefaultTxTiming}
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.parameters.{GenHeadParams, generateHeadParameters}
import hydrozoa.config.head.rulebased.{DisputeResolutionConfigGen, generateDisputeResolutionConfig}
import hydrozoa.config.head.{HeadConfig, HeadConfigGen}
import hydrozoa.config.node.operation.evacuation.{NodeOperationEvacuationConfigGen, generateNodeOperationEvacuationConfig}
import hydrozoa.config.node.operation.multisig.{NodeOperationMultisigConfig, generateNodeOperationMultisigConfig}
import hydrozoa.config.node.owninfo.OwnHeadPeerPrivate
import hydrozoa.config.{ScriptReferenceUtxosGen, generateScriptReferenceUtxos}
import hydrozoa.lib.cardano.scalus.ShelleyAddressExtra
import hydrozoa.lib.cardano.scalus.txbuilder.Transaction.attachVKeyWitnesses
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.block.Block.MultiSigned
import hydrozoa.multisig.ledger.block.BlockHeader
import org.scalacheck.util.Pretty
import org.scalacheck.{Gen, Prop, Properties, PropertyM}
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.{AddrKeyHash, SlotConfig, Transaction, VKeyWitness}
import scalus.uplc.builtin.Builtins.blake2b_224
import test.{TestM, TestMFixedEnv, TestPeers, TestPeersSpec}

/** Multi-node config is a tool for test suites that allows multisigning effects as well as giving
  * the access to the head config, which is common for all peers.
  */
// TODO: Should we add a mock cardano backend that is aware of transactions deploying the reference script utxos?
case class MultiNodeConfig private (
    nodePrivateConfigs: Map[HeadPeerNumber, NodePrivateConfig],
    override val headConfig: HeadConfig,
) extends HeadConfig.Section {
    lazy val nodeConfigs: Map[HeadPeerNumber, NodeConfig] =
        nodePrivateConfigs.map((n, pc) =>
            n ->
                NodeConfig(
                  headConfig,
                  pc.ownHeadWallet,
                  pc.nodeOperationEvacuationConfig,
                  pc.nodeOperationMultisigConfig,
                  pc.scriptReferenceUtxos
                ).get
        )

    override def headConfigPreinit: HeadConfig.Preinit = headConfig.headConfigPreinit
    override def initialBlock: MultiSigned.Initial = headConfig.initialBlock

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

    def addrKeyHashOf(peerNumber: HeadPeerNumber): AddrKeyHash =
        AddrKeyHash(blake2b_224(nodeConfigs(peerNumber).ownHeadWallet.exportVerificationKey))

    def signTxAs(peerNumber: HeadPeerNumber): Transaction => Transaction = nodeConfigs(
      peerNumber
    ).ownHeadWallet.signTx

    // TODO: are we fine with having that here? Better place?
    def pickPeer: Gen[HeadPeerNumber] =
        Gen.choose(0, nodePrivateConfigs.size - 1)
            .map(HeadPeerNumber.apply)

}

object MultiNodeConfig {
    given tooLongPretty: (MultiNodeConfig => Pretty) = _ =>
        Pretty(_ => "MultiNodeConfig (too long to print)")
    type MultiNodeConfigTestM[A] = TestM[MultiNodeConfig, A]
    private val mnctm = TestMFixedEnv[MultiNodeConfig]()
    export mnctm.*

    def runDefault[A](testM: MultiNodeConfigTestM[A])(using
        toProp: A => Prop,
        ioRuntime: IORuntime
    ): Prop =
        run(initializer = PropertyM.pick(generateDefault), testM = testM)

    def generateDefault: Gen[MultiNodeConfig] = generate(TestPeersSpec.default)()

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
        generateNodeOperationEvacuationConfig: NodeOperationEvacuationConfigGen =
            generateNodeOperationEvacuationConfig,
        generateNodeOperationMultisigConfig: Gen[NodeOperationMultisigConfig] =
            generateNodeOperationMultisigConfig,
        generateScriptReferenceUtxos: ScriptReferenceUtxosGen = generateScriptReferenceUtxos
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
          generateNodeOperationEvacuationConfig,
          generateNodeOperationMultisigConfig,
          generateScriptReferenceUtxos
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
        generateNodeOperationEvacuationConfig: NodeOperationEvacuationConfigGen =
            generateNodeOperationEvacuationConfig,
        generateNodeOperationMultisigConfig: Gen[NodeOperationMultisigConfig] =
            generateNodeOperationMultisigConfig,
        generateScriptReferenceUtxos: ScriptReferenceUtxosGen = generateScriptReferenceUtxos
    ): Gen[MultiNodeConfig] =
        for {
            headConfig <- generateHeadConfig(testPeers)(
              generateBlockCreationEndTime = generateBlockCreationEndTime,
              generateTxTiming = generateTxTiming,
              generateFallbackContingency = generateFallbackContingency,
              generateDisputeResolutionConfig = generateDisputeResolutionConfig,
              generateHeadParameters = generateHeadParameters,
              generateInitializationParameters = generateInitializationParameters,
            )

            nodePrivateConfigs <-
                Gen.sequence[List[
                  (HeadPeerNumber, NodePrivateConfig)
                ], (HeadPeerNumber, NodePrivateConfig)](
                  testPeers.headPeerIds.toList.map(peerId =>
                      for {
                          nomc <- generateNodeOperationMultisigConfig
                          ohpp = OwnHeadPeerPrivate(
                            testPeers.walletFor(peerId._1),
                            headConfig.headPeers
                          ).get
                          noec <- generateNodeOperationEvacuationConfig(ohpp.ownHeadWallet)
                          sru <- generateScriptReferenceUtxos(headConfig)
                      } yield peerId._1 -> NodePrivateConfig(
                        ownHeadPeerPrivate = ohpp,
                        // Re-using the same wallet for now, don't know if this will work
                        nodeOperationEvacuationConfig = noec,
                        nodeOperationMultisigConfig = nomc,
                        scriptReferenceUtxos = sru
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
