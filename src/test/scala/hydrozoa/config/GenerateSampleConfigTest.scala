package hydrozoa.config

import cats.effect.*
import cats.effect.unsafe.implicits.global
import fs2.io.file.Files as Fs2Files
import fs2.text
import hydrozoa.config.GenerateSampleConfig.{defaultSpec, writeAll}
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.config.node.owninfo.{OwnHeadPeerPrivate, OwnPeerPrivate}
import hydrozoa.config.node.{MultiNodeConfig, NodePrivateConfig}
import hydrozoa.lib.cardano.scalus.codecs.json.Codecs.dummySigningKey
import hydrozoa.multisig.backend.cardano.{CardanoBackendMock, MockState}
import hydrozoa.multisig.consensus.peer.PeerWallet
import io.circe.parser
import monocle.syntax.all.{as as _, *}
import org.scalacheck.Properties

object GenerateSampleConfigTest extends Properties("GenerateSampleConfig") {
    import MultiNodeConfig.*

    private def mkDummyWallet(w: PeerWallet): PeerWallet =
        PeerWallet.scalusWallet(w.exportVerificationKey, dummySigningKey)

    private def mkDummy(npc: NodePrivateConfig, headPeers: HeadPeers): NodePrivateConfig = {
        val headWallet = npc.ownPeerPrivate match {
            case h: OwnHeadPeerPrivate => h.ownHeadWallet
            case other: OwnPeerPrivate =>
                throw IllegalStateException(s"Expected head identity, got $other")
        }
        npc.focus(_.ownPeerPrivate)
            .replace(OwnHeadPeerPrivate(headWallet, headPeers).get)
            .focus(_.nodeOperationEvacuationConfig.evacuationWallet)
            .modify(mkDummyWallet)
    }

    val writeAllRoundTrip: MultiNodeConfigTestM[Boolean] = for {
        mnc <- ask

        tmpFsDir <- lift(Fs2Files[IO].createTempDirectory)
        spec = defaultSpec.copy(outDir = tmpFsDir.toNioPath, nPeers = mnc.nodePrivateConfigs.size)
        _ <- lift(writeAll(spec, mnc))

        // HeadConfig round-trip
        headJson <- lift(
          Fs2Files[IO]
              .readAll(tmpFsDir / "head-config.json")
              .through(text.utf8.decode)
              .compile
              .string
        )
        cardanoBackend <- lift(
          CardanoBackendMock.mockIO(
            MockState(initialUtxos =
                mnc.headConfig.initializationTx.resolvedUtxos.utxos
                    ++ Map.from(mnc.headConfig.scriptReferenceUtxos.toList.map(_.toTuple))
            )
          )
        )
        decodedHead <- lift(HeadConfig.fromJson(headJson, cardanoBackend).value)
        decodedHeadConfig <- failLeft(decodedHead)
        _ <- assertWith(
          mnc.headConfig == decodedHeadConfig,
          "HeadConfig should round-trip through writeAll"
        )

        // NodePrivateConfig round-trip for each peer
        _ <- lift {
            given (HeadPeers.Section & CardanoNetwork.Section) = mnc.headConfig
            mnc.nodePrivateConfigs.toList.foldLeft(IO.unit) { case (acc, (peerNum, npc)) =>
                acc >> (for {
                    npcJson <-
                        Fs2Files[IO]
                            .readAll(tmpFsDir / s"peer-$peerNum" / "private.json")
                            .through(text.utf8.decode)
                            .compile
                            .string
                    decoded <- IO.fromEither(parser.decode[NodePrivateConfig](npcJson))
                    dummy = mkDummy(npc, mnc.headConfig.headPeers)
                    _ <-
                        if dummy.nodeOperationEvacuationConfig == decoded.nodeOperationEvacuationConfig
                        then IO.unit
                        else
                            IO.raiseError(
                              AssertionError(
                                s"NodePrivateConfig for peer $peerNum failed round-trip"
                              )
                            )
                } yield ())
            }
        }
    } yield true

    val _ = property("round-trips through disk") = runDefault(writeAllRoundTrip)
}
