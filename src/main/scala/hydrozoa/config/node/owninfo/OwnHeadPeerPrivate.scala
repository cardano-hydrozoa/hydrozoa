package hydrozoa.config.node.owninfo

import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.multisig.consensus.peer.HeadPeerWallet.dummyHeadPeerWalletEncoder
import hydrozoa.multisig.consensus.peer.{HeadPeerId, HeadPeerNumber, HeadPeerWallet}
import io.circe.*
import io.circe.generic.semiauto.*
import io.circe.syntax.*
import scalus.crypto.ed25519.VerificationKey

final case class OwnHeadPeerPrivate private (
    override val ownHeadWallet: HeadPeerWallet,
    override val ownHeadPeerPublic: OwnHeadPeerPublic,
) extends OwnHeadPeerPrivate.Section {
    override def ownHeadPeerPrivate: OwnHeadPeerPrivate = this
}

object OwnHeadPeerPrivate {
    def apply(ownHeadWallet: HeadPeerWallet, headPeers: HeadPeers): Option[OwnHeadPeerPrivate] =
        val ownPeerNum: HeadPeerNumber = ownHeadWallet.getPeerNum
        val walletKey: VerificationKey = ownHeadWallet.exportVerificationKey
        for {
            ownHeadPeerPublic <- OwnHeadPeerPublic(ownPeerNum, headPeers)
            _ <- Option.when(walletKey == ownHeadPeerPublic.ownHeadVKey)(())
        } yield new OwnHeadPeerPrivate(ownHeadWallet, ownHeadPeerPublic)

    trait Section extends OwnHeadPeerPublic.Section {
        def ownHeadPeerPrivate: OwnHeadPeerPrivate

        def ownHeadWallet: HeadPeerWallet

        override def ownHeadPeerId: HeadPeerId = ownHeadPeerPublic.ownHeadPeerId
        override def ownHeadPeerNum: HeadPeerNumber = ownHeadPeerPublic.ownHeadPeerNum
        override def ownHeadVKey: VerificationKey = ownHeadPeerPublic.ownHeadVKey
    }

    given dummyOwnHeadPeerPrivateEncoder: Encoder[OwnHeadPeerPrivate] =
        Encoder.instance(ownHeadPeerPrivate =>
            Json.obj(
              "ownHeadWallet" -> ownHeadPeerPrivate.ownHeadWallet.asJson(using
                dummyHeadPeerWalletEncoder
              ),
            )
        )

    given ownHeadPeerPrivateDecoder(using
        peers: HeadPeers.Section
    ): Decoder[OwnHeadPeerPrivate] = Decoder
        .instance(c =>
            for {
                wallet <- c.downField("ownHeadWallet").as[HeadPeerWallet]
            } yield OwnHeadPeerPrivate(wallet, peers.headPeers)
        )
        .emap {
            case Some(ownHeadPeerPrivate: OwnHeadPeerPrivate) => Right(ownHeadPeerPrivate)
            case None =>
                Left(
                  "Could not construct head peer private section. Does the wallet's peer number correspond to " +
                      "an existing HeadPeerNumber?"
                )
        }
}
