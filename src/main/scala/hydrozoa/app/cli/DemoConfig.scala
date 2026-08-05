package hydrozoa.app.cli

import cats.effect.IO
import hydrozoa.config.head.initialization.InitializationParameters.HeadId
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.multisig.consensus.peer.PeerWallet
import io.circe.parser
import java.nio.file.{Files, Path}
import scalus.crypto.ed25519.{SigningKey, VerificationKey}
import scalus.uplc.builtin.ByteString

/** The minimal, offline slice of the configs that [[SubmitL2Transaction]] needs: the network (its
  * protocol parameters are static per network), the head id (for the L2 tx's headId pin), and the
  * peer's signing wallet. Deliberately avoids [[hydrozoa.config.node.NodeConfig.load]], which
  * builds a Blockfrost backend and resolves the reference-script utxos through it — an L1 round
  * trip an L2-only flow has no business making.
  */
object DemoConfig {

    final case class L2Demo(
        cardanoNetwork: CardanoNetwork,
        headId: HeadId,
        wallet: PeerWallet
    )

    /** Read the network + head id from the head config and the wallet from the private config —
      * pure file reads, no backend.
      */
    def loadOffline(headConfigPath: Path, privateConfigPath: Path): IO[L2Demo] =
        for {
            headJson <- IO
                .blocking(Files.readString(headConfigPath))
                .flatMap(s => IO.fromEither(parser.parse(s)))
            cardanoNetwork <- IO.fromEither(
              headJson.hcursor.get[CardanoNetwork]("cardanoNetwork")
            )
            headId <- IO.fromEither(headJson.hcursor.get[HeadId]("headId"))
            wallet <- readWallet(privateConfigPath)
        } yield L2Demo(cardanoNetwork, headId, wallet)

    /** Load the peer's signing wallet from its private config — head or coil shape. */
    def readWallet(privateConfigPath: Path): IO[PeerWallet] =
        for {
            json <- IO
                .blocking(Files.readString(privateConfigPath))
                .flatMap(s => IO.fromEither(parser.parse(s)))
            ownPeer = json.hcursor.downField("ownPeerPrivate")
            wallet <- IO.fromEither {
                def field(walletKind: String, key: String) =
                    ownPeer.downField(walletKind).get[String](key)
                for {
                    vKeyHex <- field("ownHeadWallet", "verificationKey")
                        .orElse(field("ownCoilWallet", "verificationKey"))
                    sKeyHex <- field("ownHeadWallet", "signingKey")
                        .orElse(field("ownCoilWallet", "signingKey"))
                } yield PeerWallet.scalusWallet(
                  VerificationKey.unsafeFromByteString(ByteString.fromHex(vKeyHex)),
                  SigningKey.unsafeFromByteString(ByteString.fromHex(sKeyHex))
                )
            }
        } yield wallet
}
