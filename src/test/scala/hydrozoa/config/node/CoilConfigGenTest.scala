package hydrozoa.config.node

import hydrozoa.config.head.coil.CoilPeer
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.parameters.generateHeadParameters
import hydrozoa.config.head.{generateHeadConfig, generateHeadConfigBootstrap}
import hydrozoa.multisig.consensus.peer.{HeadPeerNumber, PeerId}
import org.scalacheck.{Prop, Properties}
import test.{SeedPhrase, TestPeers, genMonad}

/** Foundation check for the coil-aware config path: a head config that lists a coil peer must
  * generate (the threshold script / initial block builder has to tolerate coils), expose the coil's
  * vkey, and let [[NodeConfig.mkCoilConfig]] derive that coil's node config.
  */
object CoilConfigGenTest extends Properties("Coil config generation") {

    private val network = CardanoNetwork.Preprod

    // Two head peers; the coil's wallet is an extra key from the same seed, beyond the head set.
    private val headPeers = TestPeers.apply(SeedPhrase.Yaci, network, 2)
    private val withCoil = TestPeers.apply(SeedPhrase.Yaci, network, 3)
    private val coilWallet = withCoil.walletFor(HeadPeerNumber(2))
    private val coilVKey = coilWallet.exportVerificationKey

    private val config: MultiNodeConfig =
        MultiNodeConfig
            .generateWith(headPeers)(
              generateHeadConfig = generateHeadConfig(
                genHeadConfigBootstrap = generateHeadConfigBootstrap(
                  generateHeadParams = generateHeadParameters().map(_.copy(coilQuorum = 1)),
                  coilPeers = List(CoilPeer(coilVKey, HeadPeerNumber(0)))
                )
              )
            )
            .sample
            .get

    val _ = property("head config lists exactly the one coil vkey") =
        Prop(config.headConfig.coilPeerVKeys == List(coilVKey)) :|
            s"coilPeerVKeys: ${config.headConfig.coilPeerVKeys}"

    val _ = property("the coil is hubbed by head 0") =
        Prop(config.headConfig.hubbedCoilNums(HeadPeerNumber(0)).nonEmpty) :|
            s"hubbedCoilNums(0): ${config.headConfig.hubbedCoilNums(HeadPeerNumber(0))}"

    val _ = property("mkCoilConfig derives a coil node config from the coil wallet") = {
        val head0Private = config.nodePrivateConfigs(HeadPeerNumber(0))
        val coilNodeConfig = NodeConfig
            .mkCoilConfig(
              headConfig = config.headConfig,
              ownCoilWallet = coilWallet,
              nodeOperationEvacuationConfig = head0Private.nodeOperationEvacuationConfig,
              nodeOperationMultisigConfig = head0Private.nodeOperationMultisigConfig,
              hydrozoaHost = "localhost",
              hydrozoaPort = "4973",
              blockfrostApiKey = "not-a-real-key"
            )
        Prop(coilNodeConfig.exists(_.ownPeerId.isInstanceOf[PeerId.Coil])) :|
            s"mkCoilConfig produced: ${coilNodeConfig.map(_.ownPeerId)}"
    }
}
