package hydrozoa.config.node.owninfo

import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.multisig.consensus.peer.PeerWallet
import hydrozoa.multisig.consensus.peer.PeerWallet.dummyPeerWalletEncoder
import io.circe.*
import io.circe.syntax.*

final case class OwnHeadPeerPrivate private (
    ownHeadWallet: PeerWallet,
    ownHeadPeerPublic: OwnHeadPeerPublic,
) extends OwnPeerPrivate {
    export ownHeadPeerPublic.{ownPeerId, canLeadFast, canLeadSlow, nextOwnLeaderBlock, nextOwnSlowLeaderStack, ownPeerLabel, ownPeerIndex}

    override def ownWallet: PeerWallet = ownHeadWallet
}

object OwnHeadPeerPrivate {

    /** Build a head node's private identity from its signing wallet, deriving which head peer it is
      * by matching the wallet's verification key against the head set. `None` if the key is not a
      * configured head peer.
      */
    def apply(ownHeadWallet: PeerWallet, headPeers: HeadPeers): Option[OwnHeadPeerPrivate] =
        for {
            ownHeadPeerPublic <- OwnHeadPeerPublic(ownHeadWallet.exportVerificationKey, headPeers)
        } yield new OwnHeadPeerPrivate(ownHeadWallet, ownHeadPeerPublic)

    given dummyOwnHeadPeerPrivateEncoder: Encoder[OwnHeadPeerPrivate] =
        Encoder.instance(ownHeadPeerPrivate =>
            Json.obj(
              "ownHeadWallet" -> ownHeadPeerPrivate.ownHeadWallet.asJson(using
                dummyPeerWalletEncoder
              ),
            )
        )

    given ownHeadPeerPrivateDecoder(using
        peers: HeadPeers.Section
    ): Decoder[OwnHeadPeerPrivate] = Decoder
        .instance(c =>
            for {
                wallet <- c.downField("ownHeadWallet").as[PeerWallet]
            } yield OwnHeadPeerPrivate(wallet, peers.headPeers)
        )
        .emap {
            case Some(ownHeadPeerPrivate: OwnHeadPeerPrivate) => Right(ownHeadPeerPrivate)
            case None =>
                Left(
                  "Could not construct head peer private section. Is the wallet's verification key " +
                      "among the configured head peers?"
                )
        }
}
