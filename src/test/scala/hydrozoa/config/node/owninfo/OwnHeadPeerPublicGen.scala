package hydrozoa.config.node.owninfo

import hydrozoa.config.head.HeadPeersSpec
import org.scalacheck.{Gen, Prop, Properties}
import test.GeneratorTests.property

def generateOwnHeadPeerPublic(headPeersSpec: HeadPeersSpec): Gen[OwnHeadPeerPublic] =
    for {
        testPeers <- headPeersSpec.generate
        ownPeerNumber <- Gen.oneOf(testPeers.headPeerNums.toList)
    } yield OwnHeadPeerPublic(
      ownHeadPeerNum = ownPeerNumber,
      headPeers = testPeers.headPeers
    ).get

object GenOwnHeadPeerPublic extends Properties("sanity check") {
    val _ = property("sanity check") =
        Prop.forAll(generateOwnHeadPeerPublic(headPeersSpec = HeadPeersSpec.Random))(_ => true)
}
