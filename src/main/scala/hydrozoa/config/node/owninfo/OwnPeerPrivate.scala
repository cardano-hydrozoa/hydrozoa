package hydrozoa.config.node.owninfo

import hydrozoa.config.head.coil.CoilPeers
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

    /** Decode either peer identity, dispatching on which wallet field is present: `ownHeadWallet`
      * decodes an [[OwnHeadPeerPrivate]] (vkey located among the head peers), `ownCoilWallet` an
      * [[OwnCoilPeerPrivate]] (vkey located among the coil peers supplied via [[CoilPeers]]).
      */
    given ownPeerPrivateDecoder(using
        headPeers: HeadPeers.Section,
        coilPeers: CoilPeers
    ): Decoder[OwnPeerPrivate] =
        Decoder.instance { c =>
            if c.downField("ownHeadWallet").succeeded then
                OwnHeadPeerPrivate.ownHeadPeerPrivateDecoder
                    .map((p: OwnHeadPeerPrivate) => p: OwnPeerPrivate)(c)
            else if c.downField("ownCoilWallet").succeeded then
                OwnCoilPeerPrivate.ownCoilPeerPrivateDecoder
                    .map((p: OwnCoilPeerPrivate) => p: OwnPeerPrivate)(c)
            else
                Left(
                  DecodingFailure(
                    "ownPeerPrivate must carry either ownHeadWallet or ownCoilWallet",
                    c.history
                  )
                )
        }
}
