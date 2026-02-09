package hydrozoa.config.head.peers

import org.scalacheck.Arbitrary
import test.genTestPeers

implicit lazy val headPeersArbitrary : Arbitrary[HeadPeers] =
    Arbitrary(for {
        testPeers <- genTestPeers()
        peerVKeys = testPeers.map(_.wallet.exportVerificationKey)
    } yield HeadPeers(peerVKeys))