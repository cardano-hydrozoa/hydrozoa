package hydrozoa.config.node.owninfo

// TODO:
//import hydrozoa.config.head.HeadPeersSpec
//import hydrozoa.config.node.owninfo.GenOwnHeadPeerPublic.property
//import org.scalacheck.{Gen, Prop, Properties}
//import test.SeedPhrase
//
///** TODO: we can always create it, do we generate anything here?
//  * @param testPeersSpec
//  * @return
//  */
//def generateOwnHeadPeerPrivate(testPeersSpec: HeadPeersSpec): Gen[OwnHeadPeerPrivate] =
//    for {
//        testPeers <- testPeersSpec.generate
//        ownPeerId <- Gen.oneOf(testPeers.headPeerIds.toList)
//        ownPeer = testPeers._headPeersName.toList.toMap.get(ownPeerId).get
//        wallet = testPeers.wallet(ownPeer)
//    } yield OwnHeadPeerPrivate(ownHeadWallet = wallet, headPeers = testPeers.headPeers).get
//
//object GenOwnHeadPeerPrivate extends Properties("sanity check") {
//    val _ = property("sanity check") = Prop.forAll(
//      generateOwnHeadPeerPrivate(testPeersSpec = HeadPeersSpec.Random(SeedPhrase.Yaci))
//    )(_ => true)
//}
