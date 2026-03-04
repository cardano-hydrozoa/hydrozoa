package hydrozoa.config.node.owninfo

// TODO
//import hydrozoa.config.head.HeadPeersSpec
//import org.scalacheck.{Gen, Prop, Properties}
//import test.GeneratorTests.property
//import test.SeedPhrase
//
///** TODO: we can always create it, do we generate anything here? */
//def generateOwnHeadPeerPublic(testPeersSpec: HeadPeersSpec): Gen[OwnHeadPeerPublic] =
//    for {
//        testPeers <- testPeersSpec.generate
//        ownPeerNumber <- Gen.oneOf(testPeers.headPeerNums.toList)
//    } yield OwnHeadPeerPublic(
//      ownHeadPeerNum = ownPeerNumber,
//      headPeers = testPeers.headPeers
//    ).get
//
//object GenOwnHeadPeerPublic extends Properties("sanity check") {
//    val _ = property("sanity check") =
//        Prop.forAll(generateOwnHeadPeerPublic(testPeersSpec = HeadPeersSpec.Random(SeedPhrase.Yaci)))(_ => true)
//}
