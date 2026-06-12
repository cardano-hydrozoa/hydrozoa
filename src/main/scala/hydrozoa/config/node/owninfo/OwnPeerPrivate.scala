package hydrozoa.config.node.owninfo

import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.config.node.owninfo.OwnCoilPeerPrivate.dummyOwnCoilPeerPrivateEncoder
import hydrozoa.config.node.owninfo.OwnHeadPeerPrivate.dummyOwnHeadPeerPrivateEncoder
import hydrozoa.multisig.consensus.peer.PeerWallet
import io.circe.*
import io.circe.syntax.*

/** The private own-peer surface shared by head and coil nodes: the [[OwnPeerPublic]] identity plus
  * a signing wallet. Concrete implementations: [[OwnHeadPeerPrivate]] / [[OwnCoilPeerPrivate]].
  */
trait OwnPeerPrivate extends OwnPeerPublic {

    /** This peer's signing wallet. */
    def ownWallet: PeerWallet
}

object OwnPeerPrivate {
    trait Section extends OwnPeerPublic.Section {
        def ownPeerPrivate: OwnPeerPrivate

        override def ownPeerPublic: OwnPeerPublic = ownPeerPrivate

        def ownWallet: PeerWallet = ownPeerPrivate.ownWallet
    }

    given ownPeerPrivateEncoder: Encoder[OwnPeerPrivate] = Encoder.instance {
        case h: OwnHeadPeerPrivate => h.asJson(using dummyOwnHeadPeerPrivateEncoder)
        case c: OwnCoilPeerPrivate => c.asJson(using dummyOwnCoilPeerPrivateEncoder)
    }

    // Coil nodes are constructed programmatically; the JSON decode path currently produces only a
    // head identity (wrapped in the sum). A coil JSON entry lands with the coil bootstrap path.
    given ownPeerPrivateDecoder(using HeadPeers.Section): Decoder[OwnPeerPrivate] =
        OwnHeadPeerPrivate.ownHeadPeerPrivateDecoder
            .map((p: OwnHeadPeerPrivate) => p: OwnPeerPrivate)
}
