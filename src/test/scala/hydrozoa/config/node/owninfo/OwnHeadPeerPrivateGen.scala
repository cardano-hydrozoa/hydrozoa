package hydrozoa.config.node.owninfo

import hydrozoa.config.head.HeadPeersSpec
import hydrozoa.config.node.owninfo.GenOwnHeadPeerPublic.property
import org.scalacheck.{Gen, Prop, Properties}

/** TODO: we can always create it, do we generate anything here?
  * @param headPeersSpec
  * @return
  */
def generateOwnHeadPeerPrivate(headPeersSpec: HeadPeersSpec): Gen[OwnHeadPeerPrivate] =
    for {
        testPeers <- headPeersSpec.generate
        ownPeerId <- Gen.oneOf(testPeers.headPeerIds.toList)
        wallet = testPeers._testPeers.toList.toMap.get(ownPeerId).get.wallet
    } yield OwnHeadPeerPrivate(ownHeadWallet = wallet, headPeers = testPeers.headPeers).get

object GenOwnHeadPeerPrivate extends Properties("sanity check") {
    val _ = property("sanity check") =
        Prop.forAll(generateOwnHeadPeerPrivate(headPeersSpec = HeadPeersSpec.Random))(_ => true)
}
