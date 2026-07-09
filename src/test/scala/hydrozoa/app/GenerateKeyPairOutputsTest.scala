package hydrozoa.app

import cats.data.NonEmptyMap
import hydrozoa.app.GenerateKeyPair.Role
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.peers.{HeadPeerData, HeadPeers}
import hydrozoa.config.node.NodePrivateConfig
import hydrozoa.config.node.NodePrivateConfig.nodePrivateConfigDecoder
import hydrozoa.config.node.owninfo.{OwnCoilPeerPrivate, OwnHeadPeerPrivate}
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import io.circe.parser
import java.nio.file.{Files, Path}
import org.http4s.Uri
import org.scalatest.funsuite.AnyFunSuite
import scala.collection.immutable.SortedMap
import scalus.crypto.ed25519.VerificationKey
import scalus.uplc.builtin.ByteString

/** Pins the two file outputs keygen cooks: the roster grows into a decodable
  * [[Bootstrap.Membership]], and a template filled by [[GenerateKeyPair.fillPrivateConfig]] decodes
  * into the matching own-peer identity — head *and* coil — through
  * [[NodePrivateConfig.nodePrivateConfigDecoder]]. Uses the committed `peer-private.template.json`,
  * so it also pins the template's shape.
  */
class GenerateKeyPairOutputsTest extends AnyFunSuite {

    private def hex(digit: Char): String = digit.toString * 64

    private def vkey(digit: Char): VerificationKey =
        VerificationKey.unsafeFromByteString(ByteString.fromHex(hex(digit)))

    private val wsUri: Uri = Uri.unsafeFromString("ws://head-0:4001")

    private val headPeers: HeadPeers = HeadPeers(
      NonEmptyMap.fromMapUnsafe(
        SortedMap(
          HeadPeerNumber(0) -> HeadPeerData(vkey('0'), wsUri),
          HeadPeerNumber(1) -> HeadPeerData(vkey('1'), Uri.unsafeFromString("ws://head-1:4001"))
        )
      )
    ).get

    private val coilPeerVKeys: List[VerificationKey] = List(vkey('2'), vkey('3'))

    private val template =
        parser
            .parse(Files.readString(Path.of("peer-private.template.json")))
            .fold(e => fail(s"template does not parse: $e"), identity)

    test("appendPeer grows a roster that decodes as a Membership") {
        val roster = for {
            r0 <- GenerateKeyPair.appendPeer(
              GenerateKeyPair.emptyRoster,
              Role.Head,
              hex('0'),
              Some(wsUri),
              None,
              None
            )
            r1 <- GenerateKeyPair.appendPeer(r0, Role.Coil, hex('2'), None, Some(0), Some(1))
        } yield r1

        val membership = roster
            .flatMap(_.as[Bootstrap.Membership].left.map(_.toString))
            .fold(e => fail(s"roster does not decode: $e"), identity)

        assert(
          membership.headPeers.size == 1 &&
              membership.coilPeers.size == 1 &&
              membership.coilPeers.head.hubHeadPeerNumber == HeadPeerNumber(0) &&
              membership.coilQuorum == 1
        )
    }

    test("appendPeer rejects a coil peer whose hub is not registered") {
        val result =
            GenerateKeyPair.appendPeer(
              GenerateKeyPair.emptyRoster,
              Role.Coil,
              hex('2'),
              None,
              Some(0),
              None
            )
        assert(result.isLeft)
    }

    test("a head-filled template decodes into an OwnHeadPeerPrivate identity") {
        given HeadPeers = headPeers
        given List[VerificationKey] = coilPeerVKeys
        given CardanoNetwork = CardanoNetwork.Preview

        val filled = GenerateKeyPair
            .fillPrivateConfig(template, Role.Head, hex('1'), hex('a'), hex('b'), hex('c'))
            .fold(e => fail(s"fill failed: $e"), identity)

        val decoded = parser
            .decode[NodePrivateConfig](filled.spaces2)(using nodePrivateConfigDecoder)
            .fold(e => fail(s"decode failed: $e"), identity)

        decoded.ownPeerPrivate match {
            case p: OwnHeadPeerPrivate =>
                assert(
                  p.ownPeerIndex == 1 &&
                      p.ownWallet.exportVerificationKey == vkey('1') &&
                      decoded.ruleBasedWallet.exportVerificationKey == vkey('b')
                )
            case other => fail(s"expected OwnHeadPeerPrivate, got $other")
        }
    }

    test("a coil-filled template decodes into an OwnCoilPeerPrivate identity") {
        given HeadPeers = headPeers
        given List[VerificationKey] = coilPeerVKeys
        given CardanoNetwork = CardanoNetwork.Preview

        val filled = GenerateKeyPair
            .fillPrivateConfig(template, Role.Coil, hex('3'), hex('a'), hex('b'), hex('c'))
            .fold(e => fail(s"fill failed: $e"), identity)

        val decoded = parser
            .decode[NodePrivateConfig](filled.spaces2)(using nodePrivateConfigDecoder)
            .fold(e => fail(s"decode failed: $e"), identity)

        decoded.ownPeerPrivate match {
            case p: OwnCoilPeerPrivate =>
                assert(p.ownPeerIndex == 1 && p.ownPeerLabel == "c1")
            case other => fail(s"expected OwnCoilPeerPrivate, got $other")
        }
    }
}
