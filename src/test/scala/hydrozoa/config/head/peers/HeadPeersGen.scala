package hydrozoa.config.head.peers

import cats.data.NonEmptyList
import hydrozoa.config.node.NodeConfig
import hydrozoa.lib.cardano.scalus.txbuilder.Transaction.attachVKeyWitnesses
import hydrozoa.lib.number.PositiveInt
import hydrozoa.multisig.consensus.peer.{HeadPeerId, HeadPeerNumber}
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import org.scalacheck.Gen
import scalus.cardano.ledger.{Transaction, VKeyWitness}
import scalus.crypto.ed25519.VerificationKey
import scalus.|>
import test.TestPeer

final case class TestPeers(
    _testPeers: NonEmptyList[(HeadPeerId, TestPeer)]
) extends TestPeers.Section {
    val headPeers: HeadPeers = HeadPeers(headPeerVKeys)

    override def headPeerNums: NonEmptyList[HeadPeerNumber] = _testPeers.map(_._1._1)

    override def headPeerIds: NonEmptyList[HeadPeerId] = _testPeers.map(_._1)

    override def headPeerVKeys: NonEmptyList[VerificationKey] =
        _testPeers.map(_._2.wallet.exportVerificationKey)

    override def headPeerVKey(n: HeadPeerNumber): Option[VerificationKey] =
        _testPeers.find(_._1._1 == n).map(_._2.wallet.exportVerificationKey)

    override def headPeerVKey(id: HeadPeerId): Option[VerificationKey] =
        _testPeers.find(_._1 == id).map(_._2.wallet.exportVerificationKey)

    override def headMultisigScript: HeadMultisigScript = HeadMultisigScript(headPeers)

    override def nHeadPeers: PositiveInt = PositiveInt.unsafeApply(_testPeers.size)

    override def mkVKeyWitnesses(tx: Transaction): NonEmptyList[VKeyWitness] =
        _testPeers.map(_._2.wallet.mkVKeyWitness(tx))
}

object TestPeers {

    def apply(list: List[TestPeer]): TestPeers =
        val nel = NonEmptyList.fromListUnsafe(list)
        TestPeers(
          nel.map(e =>
              val peerId = e.ordinal |> HeadPeerNumber.apply |> (n => HeadPeerId(n, nel.size))
              peerId -> e
          )
        )

    trait Section extends HeadPeers.Section {

        def mkVKeyWitnesses(tx: Transaction): NonEmptyList[VKeyWitness]

        def multisignTx(tx: Transaction): Transaction =
            tx.attachVKeyWitnesses(mkVKeyWitnesses(tx).toList)

        // def mkMinorHeaderSignature(headerSerialized: BlockHeader.Minor.Onchain.Serialized): Unit = ???
        // def multisignHeader(headerSerialized: BlockHeader.Minor.Onchain.Serialized): Unit= ???

        def testPeerNodeConfigs(): NonEmptyList[NodeConfig] = ???
    }
}

def generateTestPeers(minPeers: Int = 1, maxPeers: Int = 20): Gen[TestPeers] = {
    require(0 < minPeers && minPeers <= TestPeer.nNamedPeers)
    require(minPeers <= maxPeers && maxPeers <= TestPeer.nNamedPeers)
    for {
        numPeers <- Gen.choose(minPeers, maxPeers)
        peers = TestPeer.peerNumRange.take(numPeers).map(TestPeer.fromOrdinal)
    } yield TestPeers.apply(peers.toList)
}
