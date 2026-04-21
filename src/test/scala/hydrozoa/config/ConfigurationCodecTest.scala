package hydrozoa.config

import cats.effect.*
import cats.effect.unsafe.implicits.global
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.head.HeadConfig.given
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.config.node.owninfo.OwnHeadPeerPrivate
import hydrozoa.config.node.{MultiNodeConfig, NodePrivateConfig}
import hydrozoa.lib.cardano.scalus.codecs.json.Codecs.dummySigningKey
import hydrozoa.multisig.backend.cardano.{CardanoBackendMock, MockState}
import hydrozoa.multisig.consensus.peer.HeadPeerWallet
import io.circe.syntax.*
import monocle.syntax.all.{as as _, *}
import org.scalacheck.Properties

object ConfigurationCodecTest extends Properties("Configuration Codec Properties") {
    import MultiNodeConfig.*

//    override def overrideParameters(p: Test.Parameters): Test.Parameters =
//        p.withInitialSeed(Seed.fromBase64("AIUK99d5Zgz2qjvWyA8NCvvkh9Q5mbFWbux4o6hfQZG=").get)

    val headConfigRoundTrip: MultiNodeConfigTestM[Boolean] =
        for {
            mnc <- ask
            headConfig = mnc.headConfig
            encoded = headConfig.asJson
            encodedString = encoded.toString

            cardanoBackend <- lift(
              CardanoBackendMock.mockIO(
                MockState(initialUtxos =
                    Map(headConfig.seedUtxo.toTuple)
                        ++ headConfig.additionalFundingUtxos
                        ++ Map.from(headConfig.scriptReferenceUtxos.toList.map(_.toTuple))
                )
              )
            )

//            _ <- lift(IO.println(encodedString))
            decodingResult <- lift(
              HeadConfig.fromJson(encodedString, cardanoBackend).value
            )
            decoded <- failLeft(decodingResult)

            _ <- assertWith(
              headConfig == decoded,
              "HeadConfig should round trip through JSON." +
                  "=" * 80 + s"\nMarshalled:\n\n $headConfig \n\n" +
                  "=" * 80 + s"\nEncoded:\n\n $encoded \n\n" +
                  "=" * 80 + s"\nDecoded:\n\n $decoded  \n\n"
            )
        } yield true

    // We don't allow direct inspection of the signing key of wallets, and therefore we cannot serialize them directly.
    // This tests replaces the signing keys with "dummy" all-zeros keys.
    val dummyPrivateConfigRoundTrip: MultiNodeConfigTestM[Unit] = {
        def mkDummyWallet(w: HeadPeerWallet): HeadPeerWallet =
            HeadPeerWallet.scalusWallet(w.getPeerNum, w.exportVerificationKey, dummySigningKey)

        def mkDummy(ncp: NodePrivateConfig, headPeers: HeadPeers): NodePrivateConfig = {
            val dummyPrivate =
                OwnHeadPeerPrivate(ncp.ownHeadPeerPrivate.ownHeadWallet, headPeers).get
            ncp.focus(_.ownHeadPeerPrivate)
                .replace(dummyPrivate)
                .focus(_.nodeOperationEvacuationConfig.evacuationWallet)
                .modify(mkDummyWallet)
        }

        for {
            mnc <- ask
            _ <- {
                given (HeadPeers.Section & CardanoNetwork.Section) = mnc.headConfig
                val npc = mnc.nodePrivateConfigs.head._2
                val dummy = mkDummy(npc, mnc.headPeers)
                val encoded = dummy.asJson
                for {
//                    _ <- lift(IO.println(encoded))
                    decoded <- failLeft(encoded.as[NodePrivateConfig])
                    _ <- assertWith(
                      dummy.nodeOperationEvacuationConfig == decoded.nodeOperationEvacuationConfig,
                      "NodePrivateConfig should round trip through JSON." +
                          "=" * 80 + s"\nMarshalled (dummy):\n\n $dummy \n\n" +
                          "=" * 80 + s"\nEncoded:\n\n $encoded \n\n" +
                          "=" * 80 + s"\nDecoded:\n\n $decoded \n\n"
                    )
                } yield ()
            }

        } yield ()
    }

    val _ = property("round tripping") = runDefault(
      for {
          _ <- headConfigRoundTrip
          _ <- dummyPrivateConfigRoundTrip
      } yield true
    )
}
