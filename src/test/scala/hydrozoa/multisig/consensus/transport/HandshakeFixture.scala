package hydrozoa.multisig.consensus.transport

import hydrozoa.config.head.coil.CoilPeers
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.multisig.consensus.peer.{CoilPeerNumber, HeadPeerNumber, PeerWallet}
import scalus.cardano.ledger.{Blake2b_256, Hash, Hash32}
import scalus.uplc.builtin.ByteString
import scodec.bits.ByteVector
import test.{SeedPhrase, TestPeers}

/** The identities a handshake test needs: two head peers, two coil peers hubbed by head peer 0, and
  * the head-params digest they all agree on.
  *
  * A real head's rosters, because the check under test is exactly "does this signature verify under
  * the roster key for the number claimed" — a hand-rolled key pair would prove the arithmetic and
  * not the wiring.
  */
object HandshakeFixture {

    val network: CardanoNetwork = CardanoNetwork.Preprod

    given CardanoNetwork.Section = network

    val peers: TestPeers = TestPeers(SeedPhrase.Yaci, network, peersNumber = 2, coilPeersNumber = 2)

    val headPeers: HeadPeers.Section = peers

    /** Every coil peer hubbed by head peer 0 — the topology the hub-side tests bind. */
    val coilPeers: CoilPeers = peers.coilPeersConfig(HeadPeerNumber(0))

    def headWallet(peerNum: Int): PeerWallet = peers.walletFor(HeadPeerNumber(peerNum))

    def coilWallet(coilNum: Int): PeerWallet = peers.coilWalletFor(CoilPeerNumber(coilNum))

    /** A wallet in neither roster: the impersonator. */
    val strangerWallet: PeerWallet = TestPeers.deriveScalusWallet("handshake-stranger", 0)

    /** The head-params digest both ends of a link are expected to agree on. A fixed value rather
      * than one computed from a config: nothing here exercises the preimage, only the comparison.
      */
    val headParamsHash: Hash32 = digestOf(0x11)

    /** What a peer running a different head config carries. */
    val otherHeadParamsHash: Hash32 = digestOf(0x22)

    /** A fixed nonce, for the cases that need the two ends to be looking at the same one. */
    val nonce: HandshakeNonce = nonceOf(0xaa)

    /** A second nonce, for the replay cases. */
    val otherNonce: HandshakeNonce = nonceOf(0xbb)

    def nonceOf(fill: Int): HandshakeNonce =
        HandshakeNonce(ByteVector.fill(HandshakeNonce.sizeBytes.toLong)(fill.toByte))

    private def digestOf(fill: Int): Hash32 =
        Hash[Blake2b_256, Any](ByteString.fromArray(Array.fill(32)(fill.toByte)))
}
