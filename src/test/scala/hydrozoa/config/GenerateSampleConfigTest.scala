package hydrozoa.config

import cats.effect.*
import cats.effect.unsafe.implicits.global
import fs2.io.file.Files as Fs2Files
import fs2.text
import hydrozoa.config.GenerateSampleConfig.{defaultSpec, writeAll}
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.head.coil.CoilPeers
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.config.node.owninfo.{OwnHeadPeerPrivate, OwnPeerPrivate}
import hydrozoa.config.node.{MultiNodeConfig, NodeConfig, NodePrivateConfig}
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
            .focus(_.nodeOperationEvacuationConfig.ruleBasedWallet)
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
            given CoilPeers = mnc.headConfig.coilPeers
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

        // Full NodeConfig.fromJson path — exercises the head-config pre-parses (network /
        // headPeers / coilPeers) exactly as Main does, which the direct decoder round-trips
        // above bypass.
        npc0Json <- lift(
          Fs2Files[IO]
              .readAll(tmpFsDir / s"peer-${mnc.nodePrivateConfigs.head._1}" / "private.json")
              .through(text.utf8.decode)
              .compile
              .string
        )
        decodedNode <- lift(NodeConfig.fromJson(headJson, npc0Json, Some(cardanoBackend)).value)
        _ <- failLeft(decodedNode)
    } yield true

    val _ = property("round-trips through disk") = runDefault(writeAllRoundTrip)
}
