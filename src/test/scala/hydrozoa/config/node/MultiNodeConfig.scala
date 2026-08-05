package hydrozoa.config.node

import cats.data.Kleisli.liftF
import cats.data.{NonEmptyList, ReaderT}
import cats.effect.unsafe.IORuntime
import cats.effect.{IO, Resource}
import hydrozoa.config.head.coil.{CoilPeerData, CoilPeers}
import hydrozoa.config.head.parameters.generateHeadParameters
import hydrozoa.config.head.{HeadConfig, generateHeadConfig, generateHeadConfigBootstrap}
import hydrozoa.config.node.operation.evacuation.{NodeOperationEvacuationConfigGen, generateNodeOperationEvacuationConfig}
import hydrozoa.config.node.operation.multisig.{NodeOperationMultisigConfig, generateNodeOperationMultisigConfig}
import hydrozoa.config.node.owninfo.OwnHeadPeerPrivate
import hydrozoa.lib.cardano.scalus.VerificationKeyExtra.shelleyAddress
import hydrozoa.lib.cardano.scalus.txbuilder.Transaction.attachVKeyWitnesses
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.block.BlockHeader
import hydrozoa.multisig.ledger.stack.StandaloneEvacuationCommitment
import org.scalacheck.util.Pretty
import org.scalacheck.{Gen, Prop, Properties, PropertyM}
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.{AddrKeyHash, Transaction, VKeyWitness}
import scalus.uplc.builtin.Builtins.blake2b_224
import test.{GenWithTestPeers, TestM, TestMFixedEnv, TestPeers, TestPeersSpec, given}

/** Multi-node config is a tool for test suites that allows multisigning effects as well as giving
  * the access to the head config, which is common for all peers.
  */
// TODO: Should we add a mock cardano backend that is aware of transactions deploying the reference script utxos?
case class MultiNodeConfig private (
    nodePrivateConfigs: Map[HeadPeerNumber, NodePrivateConfig],
    override val headConfig: HeadConfig,
    coilWallets: List[hydrozoa.multisig.consensus.peer.PeerWallet],
) extends HeadConfig.Section {
    lazy val nodeConfigs: Map[HeadPeerNumber, NodeConfig] =
        nodePrivateConfigs.map((n, pc) =>
            n ->
                NodeConfig
                    .mkHeadConfig(
                      headConfig = headConfig,
                      // This fixture builds head nodes; mkHeadConfig derives the head identity from the
                      // wallet's key.
                      ownHeadWallet = pc.ownWallet,
                      nodeOperationEvacuationConfig = pc.nodeOperationEvacuationConfig,
                      nodeOperationMultisigConfig = pc.nodeOperationMultisigConfig,
                      pc.blockfrostApiKey,
                      pc.remoteLedgerUri,
                      pc.adminUsername,
                      pc.adminPassword,
                      pc.httpHost,
                      pc.httpPort,
                    )
                    .get
        )

    override def headConfigBootstrap: HeadConfig.Bootstrap = headConfig.headConfigBootstrap

    def multisignTx(tx: Transaction): Transaction =
        tx.attachVKeyWitnesses(mkVKeyWitnesses(tx).toList)

    def mkVKeyWitnesses(tx: Transaction): NonEmptyList[VKeyWitness] =
        NonEmptyList.fromListUnsafe(
          nodePrivateConfigs.map(_._2.ownWallet.mkVKeyWitness(tx)).toList
        )

    def multisignHeader(
        blockHeader: StandaloneEvacuationCommitment.Onchain
    ): NonEmptyList[BlockHeader.Minor.HeaderSignature] =
        val serialized = StandaloneEvacuationCommitment.Onchain.Serialized(blockHeader)
        NonEmptyList.fromListUnsafe(
          nodePrivateConfigs.map(_._2.ownWallet.mkHeaderSignature(serialized)).toList
        )

    /** Signs `blockHeader` with the first `coilQuorum` coil wallets and returns `None` for the
      * rest, producing the sparse list expected by the on-chain coil multisig check.
      */
    def multisignHeaderCoil(
        blockHeader: StandaloneEvacuationCommitment.Onchain
    ): List[Option[BlockHeader.Minor.HeaderSignature]] =
        val serialized = StandaloneEvacuationCommitment.Onchain.Serialized(blockHeader)
        val quorum = headConfig.coilQuorum
        coilWallets.zipWithIndex.map { (w, i) =>
            if i < quorum then Some(w.mkHeaderSignature(serialized)) else None
        }

    /** The dense header-signature list persisted for a hard-confirmed SEC: every head peer's
      * signature followed by the first `coilQuorum` coil signatures. Matches the order
      * `HardAckAggregator.collectSecSignatures` produces and `RuleBasedActor.loadAction` splits at
      * `nHeadPeers` to recover head vs coil signatures.
      */
    def multisignHeaderDense(
        blockHeader: StandaloneEvacuationCommitment.Onchain
    ): List[BlockHeader.Minor.HeaderSignature] =
        val serialized = StandaloneEvacuationCommitment.Onchain.Serialized(blockHeader)
        val headSigs = nodePrivateConfigs.map(_._2.ownWallet.mkHeaderSignature(serialized)).toList
        val coilSigs = coilWallets.take(headConfig.coilQuorum).map(_.mkHeaderSignature(serialized))
        headSigs ++ coilSigs

    def addressOf(peerNumber: HeadPeerNumber): ShelleyAddress = nodeConfigs(
      peerNumber
    ).ownWallet.exportVerificationKey.shelleyAddress()(using headConfig)

    def addrKeyHashOf(peerNumber: HeadPeerNumber): AddrKeyHash =
        AddrKeyHash(blake2b_224(nodeConfigs(peerNumber).ownWallet.exportVerificationKey))

    def signTxAs(peerNumber: HeadPeerNumber): Transaction => Transaction = nodeConfigs(
      peerNumber
    ).ownWallet.signTx

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

    extension (mnc: MultiNodeConfig)
        /** Coil [[NodeConfig]]s for each of `coilWallets`, reusing head 0's operation-config
          * sections (evacuation polling period, cardano-liaison polling period, etc.) — these don't
          * depend on head-vs-coil, and the coil path doesn't exercise any wallet-derived fields on
          * the template. Placeholder strings for blockfrost / websocket / http-admin are cosmetic
          * in a mock-backed integration test. Callers get one `NodeConfig` per coil wallet in the
          * same order the wallets are passed.
          */
        def mkCoilNodeConfigs(
            coilWallets: List[hydrozoa.multisig.consensus.peer.PeerWallet]
        ): List[NodeConfig] = {
            val templatePrivate = mnc.nodePrivateConfigs(HeadPeerNumber(0))
            coilWallets.map { w =>
                NodeConfig
                    .mkCoilConfig(
                      headConfig = mnc.headConfig,
                      ownCoilWallet = w,
                      nodeOperationEvacuationConfig = templatePrivate.nodeOperationEvacuationConfig
                          .copy(ruleBasedWallet = w),
                      nodeOperationMultisigConfig = templatePrivate.nodeOperationMultisigConfig,
                      blockfrostApiKey = "not-a-real-key",
                      remoteLedgerUri = Some("ws://localhost:3001/ws"),
                      adminUsername = "admin",
                      adminPassword = "welcome",
                      httpHost = "0.0.0.0",
                      httpPort = "8080",
                    )
                    .getOrElse(
                      throw new IllegalStateException(
                        s"coil wallet ${w.exportVerificationKey} not registered in " +
                            "headConfig.coilPeerVKeys"
                      )
                    )
            }
        }

    def runDefault[A](testM: MultiNodeConfigTestM[A])(using
        toProp: A => Prop,
        ioRuntime: IORuntime
    ): Prop =
        run(
          resource = PropertyM
              .pick[IO, MultiNodeConfig](generateDefault)
              .map(Resource.pure[IO, MultiNodeConfig](_)),
          testM = testM
        )

    def runWithCoil[A](nCoil: Int = 5, quorum: Int = 3)(testM: MultiNodeConfigTestM[A])(using
        toProp: A => Prop,
        ioRuntime: IORuntime
    ): Prop =
        run(
          resource = PropertyM
              .pick[IO, MultiNodeConfig](generateWithCoil(nCoil, quorum))
              .map(Resource.pure[IO, MultiNodeConfig](_)),
          testM = testM
        )

    def generateDefault: Gen[MultiNodeConfig] = generate(TestPeersSpec.default)()

    def generate(spec: TestPeersSpec)(
        generateHeadConfig: GenWithTestPeers[HeadConfig] =
            hydrozoa.config.head.generateHeadConfig(),
        generateNodeOperationEvacuationConfig: NodeOperationEvacuationConfigGen =
            generateNodeOperationEvacuationConfig,
        generateNodeOperationMultisigConfig: HeadConfig => Gen[NodeOperationMultisigConfig] = hc =>
            generateNodeOperationMultisigConfig(hc.maxCardanoLiaisonPollingPeriod),
    ): Gen[MultiNodeConfig] = for {
        testPeers <- TestPeers.generate(spec)
        ret <- generateForTestPeers(
          generateHeadConfig,
          generateNodeOperationEvacuationConfig,
          generateNodeOperationMultisigConfig
        ).run(testPeers)
    } yield ret

    def generateWithCoil(nCoil: Int = 5, quorum: Int = 3): Gen[MultiNodeConfig] =
        for {
            testPeers <- TestPeers.generate(TestPeersSpec.default)
            allPeers = TestPeers(
              testPeers.seedPhrase,
              testPeers.cardanoNetwork,
              testPeers.peersNumber + nCoil
            )
            coilWallets = (0 until nCoil).toList.map(i =>
                allPeers.walletFor(
                  hydrozoa.multisig.consensus.peer.HeadPeerNumber(testPeers.peersNumber + i)
                )
            )
            hubNum = HeadPeerNumber(0)
            mnc <- generateForTestPeers(
              generateHeadConfig = generateHeadConfig(
                genHeadConfigBootstrap = generateHeadConfigBootstrap(
                  generateHeadParams = generateHeadParameters().map(_.copy(coilQuorum = quorum)),
                  coilPeers = CoilPeers.indexed(
                    coilWallets.map(w => CoilPeerData(w.exportVerificationKey, hubNum))
                  )
                )
              )
            ).run(testPeers)
        } yield mnc.copy(coilWallets = coilWallets)

    /** Generate MultiNodeConfig using an existing TestPeers instance. This is useful when you need
      * to use a specific TestPeers (e.g., from test environment) rather than generating a new one.
      */
    def generateWith(testPeers: TestPeers)(
        generateHeadConfig: GenWithTestPeers[HeadConfig] =
            hydrozoa.config.head.generateHeadConfig(),
        generateNodeOperationEvacuationConfig: NodeOperationEvacuationConfigGen =
            generateNodeOperationEvacuationConfig,
        generateNodeOperationMultisigConfig: HeadConfig => Gen[NodeOperationMultisigConfig] = hc =>
            generateNodeOperationMultisigConfig(hc.maxCardanoLiaisonPollingPeriod),
    ): Gen[MultiNodeConfig] =
        generateForTestPeers(
          generateHeadConfig,
          generateNodeOperationEvacuationConfig,
          generateNodeOperationMultisigConfig
        ).run(testPeers)

    def generateForTestPeers(
        generateHeadConfig: GenWithTestPeers[HeadConfig] =
            hydrozoa.config.head.generateHeadConfig(),
        generateNodeOperationEvacuationConfig: NodeOperationEvacuationConfigGen =
            generateNodeOperationEvacuationConfig,
        generateNodeOperationMultisigConfig: HeadConfig => Gen[NodeOperationMultisigConfig] = hc =>
            generateNodeOperationMultisigConfig(hc.maxCardanoLiaisonPollingPeriod),
    ): GenWithTestPeers[MultiNodeConfig] =
        for {
            testPeers <- ReaderT.ask
            headConfig <- generateHeadConfig
            nodePrivateConfigs <-
                liftF(
                  Gen.sequence[List[
                    (HeadPeerNumber, NodePrivateConfig)
                  ], (HeadPeerNumber, NodePrivateConfig)](
                    testPeers.headPeerIds.toList.map(peerId =>
                        for {
                            nomc <- generateNodeOperationMultisigConfig(headConfig)
                            ohpp = OwnHeadPeerPrivate(
                              testPeers.walletFor(peerId._1),
                              headConfig.headPeers
                            ).get
                            noec <- generateNodeOperationEvacuationConfig(ohpp.ownHeadWallet)

                        } yield peerId._1 -> NodePrivateConfig(
                          ownPeerPrivate = ohpp,
                          // Re-using the same wallet for now, don't know if this will work
                          nodeOperationEvacuationConfig = noec,
                          nodeOperationMultisigConfig = nomc,
                          blockfrostApiKey = "not a real blockfrost api key",
                          remoteLedgerUri = Some("ws://localhost:3001/ws"),
                          adminUsername = "admin",
                          adminPassword = "welcome",
                          httpHost = "0.0.0.0",
                          httpPort = "8080",
                        )
                    )
                  )
                )
        } yield new MultiNodeConfig(
          nodePrivateConfigs = nodePrivateConfigs.toMap,
          headConfig = headConfig,
          coilWallets = List.empty
        )
}

object MultiNodeConfigTest extends Properties("Multi-node config") {
    val _ = property("generates") = Prop.forAll(
      TestPeersSpec.generate().flatMap(MultiNodeConfig.generate(_)())
    )(mnc =>
        // InitialBlock now carries the UNSIGNED init+fallback txs (slow consensus signs them at
        // startup), so we just verify the init tx is present.
        mnc.initialBlock.effects.initializationTx.tx.body.value.outputs.nonEmpty
    )
}
