package hydrozoa.config.node

// TODO:
//import hydrozoa.config.head.HeadPeersSpec
//import hydrozoa.config.node.operation.liquidation.NodeOperationLiquidationConfig
//import hydrozoa.config.node.operation.multisig.NodeOperationMultisigConfig
//import hydrozoa.config.node.owninfo.OwnHeadPeerPrivate
//import hydrozoa.lib.number.PositiveInt
//import org.scalacheck.Gen
//import scala.concurrent.duration.DurationInt
//
//// NOTE: The bounds for the polling period and max events are chosen arbitrarily and may not
//// be realistic.
//def generateNodePrivateConfig(testPeersSpec: HeadPeersSpec): Gen[NodePrivateConfig] =
//    for {
//        testPeers <- testPeersSpec.generate
//        ownHeadWallet <- Gen
//            .oneOf(testPeers._headPeersName.toList)
//            .map((_, testPeer) => testPeers.wallet(testPeer))
//        liquidationPollingPeriod <- Gen.choose(10, 60).map(_.seconds)
//        cardanoLiaisonPollingPeriod <- Gen.choose(10, 60).map(_.seconds)
//        peerLiaisonMaxEvents <- Gen.choose(1, 45)
//    } yield NodePrivateConfig(
//      ownHeadPeerPrivate = OwnHeadPeerPrivate(
//        ownHeadWallet = ownHeadWallet,
//        headPeers = testPeers.headPeers
//      ).get,
//      nodeOperationLiquidationConfig = NodeOperationLiquidationConfig(
//        liquidationBotPollingPeriod = liquidationPollingPeriod
//      ),
//      nodeOperationMultisigConfig = NodeOperationMultisigConfig(
//        cardanoLiaisonPollingPeriod = cardanoLiaisonPollingPeriod,
//        peerLiaisonMaxEventsPerBatch = PositiveInt(peerLiaisonMaxEvents).get
//      )
//    )
