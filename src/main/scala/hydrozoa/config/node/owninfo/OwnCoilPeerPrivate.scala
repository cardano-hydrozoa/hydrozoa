package hydrozoa.config.node.owninfo

import hydrozoa.config.head.coil.CoilPeers
import hydrozoa.multisig.consensus.peer.PeerWallet
import hydrozoa.multisig.consensus.peer.PeerWallet.dummyPeerWalletEncoder
import io.circe.*
import io.circe.syntax.*

final case class OwnCoilPeerPrivate private (
    ownCoilWallet: PeerWallet,
    ownCoilPeerPublic: OwnCoilPeerPublic,
) extends OwnPeerPrivate {
    export ownCoilPeerPublic.{ownPeerId, canLeadFast, canLeadSlow, nextOwnLeaderBlock, nextOwnSlowLeaderStack, ownPeerLabel, ownPeerIndex}

    override def ownWallet: PeerWallet = ownCoilWallet
}

object OwnCoilPeerPrivate {

    /** Build a coil node's private identity from its signing wallet and the `coilPeers`. `None` if
      * the wallet's key is not in the configured coil set.
      */
    def apply(
        ownCoilWallet: PeerWallet,
        coilPeers: CoilPeers
    ): Option[OwnCoilPeerPrivate] =
        for {
            ownCoilPeerPublic <- OwnCoilPeerPublic(
              ownCoilWallet.exportVerificationKey,
              coilPeers
            )
        } yield new OwnCoilPeerPrivate(ownCoilWallet, ownCoilPeerPublic)

    given dummyOwnCoilPeerPrivateEncoder: Encoder[OwnCoilPeerPrivate] =
        Encoder.instance(ownCoilPeerPrivate =>
            Json.obj(
              "ownCoilWallet" -> ownCoilPeerPrivate.ownCoilWallet.asJson(using
                dummyPeerWalletEncoder
              ),
            )
        )

    given ownCoilPeerPrivateDecoder(using
        coilPeers: CoilPeers
    ): Decoder[OwnCoilPeerPrivate] = Decoder
        .instance(c =>
            for {
                wallet <- c.downField("ownCoilWallet").as[PeerWallet]
            } yield OwnCoilPeerPrivate(wallet, coilPeers)
        )
        .emap {
            case Some(ownCoilPeerPrivate: OwnCoilPeerPrivate) => Right(ownCoilPeerPrivate)
            case None =>
                Left(
                  "Could not construct coil peer private section. Is the wallet's verification key " +
                      "among the configured coil peers?"
                )
        }
}
