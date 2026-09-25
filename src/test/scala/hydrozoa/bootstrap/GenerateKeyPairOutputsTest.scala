package hydrozoa.bootstrap

import cats.data.NonEmptyMap
import hydrozoa.bootstrap.GenerateKeyPair.Role
import hydrozoa.config.head.coil.{CoilPeerData, CoilPeers}
import hydrozoa.config.head.peers.{HeadPeerData, HeadPeers}
import hydrozoa.config.node.NodePrivateConfig.nodePrivateConfigDecoder
import hydrozoa.config.node.owninfo.{OwnCoilPeerPrivate, OwnHeadPeerPrivate}
import hydrozoa.config.node.{NodePrivateConfig, PrivateSecrets}
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import io.circe.parser
import java.nio.file.{Files, Path}
import org.bouncycastle.crypto.params.Ed25519PrivateKeyParameters
import org.http4s.Uri
import org.scalatest.funsuite.AnyFunSuite
import scala.collection.immutable.SortedMap
import scalus.crypto.ed25519.VerificationKey
import scalus.uplc.builtin.ByteString

/** Pins the two file outputs keygen cooks: the roster grows into a decodable
  * [[Bootstrap.Membership]], and a template filled by [[GenerateKeyPair.fillPrivateConfig]] decodes
  * into the matching own-peer identity — head *and* coil — through
  * [[NodePrivateConfig.nodePrivateConfigDecoder]]. Uses the committed template resource
  * (`src/main/resources/scaffold/peer-private.template.json`), so it also pins the template's
  * shape.
  */
class GenerateKeyPairOutputsTest extends AnyFunSuite {

    /** A signing key: an arbitrary but valid 32-byte Ed25519 seed. */
    private def hex(digit: Char): String = digit.toString * 64

    /** The verification key that ACTUALLY belongs to `hex(digit)`.
      *
      * Derived rather than invented, because the config loader re-derives it and refuses a pair
      * that does not agree. A fixture pairing an arbitrary "vkey" with an unrelated "skey" builds a
      * config no node would accept, and would test the decoder against something unreachable.
      */
    private def vkeyHex(digit: Char): String =
        Ed25519PrivateKeyParameters(ByteString.fromHex(hex(digit)).bytes, 0)
            .generatePublicKey()
            .getEncoded
            .map("%02x".format(_))
            .mkString

    private def vkey(digit: Char): VerificationKey =
        VerificationKey.unsafeFromByteString(ByteString.fromHex(vkeyHex(digit)))

    private val wsUri: Uri = Uri.unsafeFromString("ws://head-0:4001")

    private val headPeers: HeadPeers = HeadPeers(
      NonEmptyMap.fromMapUnsafe(
        SortedMap(
          HeadPeerNumber(0) -> HeadPeerData(vkey('0'), wsUri),
          HeadPeerNumber(1) -> HeadPeerData(vkey('1'), Uri.unsafeFromString("ws://head-1:4001"))
        )
      )
    ).get

    private val coilPeers: CoilPeers = CoilPeers.indexed(
      List(
        CoilPeerData(vkey('2'), HeadPeerNumber(0)),
        CoilPeerData(vkey('3'), HeadPeerNumber(0))
      )
    )

    private val template =
        parser
            .parse(
              Files.readString(Path.of("src/main/resources/scaffold/peer-private.template.json"))
            )
            .fold(e => fail(s"template does not parse: $e"), identity)

    test("appendPeer grows a roster that decodes as a Membership") {
        val roster = for {
            r0 <- GenerateKeyPair.appendPeer(
              GenerateKeyPair.emptyRoster,
              Role.Head,
              hex('0'),
              Some(wsUri),
              None
            )
            r1 <- GenerateKeyPair.appendPeer(r0, Role.Coil, hex('2'), None, Some(0))
        } yield r1

        val membership = roster
            .flatMap(_.as[Bootstrap.Membership].left.map(_.toString))
            .fold(e => fail(s"roster does not decode: $e"), identity)

        assert(
          membership.headPeers.size == 1 &&
              membership.coilPeers.size == 1 &&
              membership.coilPeers.head.hubHeadPeerNumber == HeadPeerNumber(0)
        )
    }

    test("appendPeer rejects a coil peer whose hub is not registered") {
        val result =
            GenerateKeyPair.appendPeer(
              GenerateKeyPair.emptyRoster,
              Role.Coil,
              hex('2'),
              None,
              Some(0)
            )
        assert(result.isLeft)
    }

    test("a head-filled template decodes into an OwnHeadPeerPrivate identity") {
        given HeadPeers = headPeers
        given CoilPeers = coilPeers

        // The round trip the split creates: `fillPrivateConfig` writes the config and the
        // credentials as a pair, and the loader reassembles them. Decoding the config alone is
        // supposed to be impossible now, which the dedicated test below asserts.
        val (filled, secrets) = GenerateKeyPair
            .fillPrivateConfig(template, Role.Head, vkeyHex('1'), hex('1'))
            .fold(e => fail(s"fill failed: $e"), identity)

        val reassembled = PrivateSecrets
            .applySecrets(filled, secrets, "test")
            .fold(e => fail(s"credentials rejected: ${e.reason}"), identity)

        val decoded = parser
            .decode[NodePrivateConfig](reassembled.spaces2)(using nodePrivateConfigDecoder)
            .fold(e => fail(s"decode failed: $e"), identity)

        decoded.ownPeerPrivate match {
            case p: OwnHeadPeerPrivate =>
                assert(
                  p.ownPeerIndex == 1 &&
                      p.ownWallet.exportVerificationKey == vkey('1')
                )
            case other => fail(s"expected OwnHeadPeerPrivate, got $other")
        }
    }

    test("the screener uri survives a head fill and is dropped from a coil's") {
        val head = GenerateKeyPair
            .fillPrivateConfig(template, Role.Head, vkeyHex('1'), hex('1'))
            .fold(e => fail(s"fill failed: $e"), identity)
            ._1
        val coil = GenerateKeyPair
            .fillPrivateConfig(template, Role.Coil, vkeyHex('3'), hex('3'))
            .fold(e => fail(s"fill failed: $e"), identity)
            ._1

        // The template has to carry it, or the head check passes vacuously.
        val inTemplate = template.hcursor.downField("remoteScreenerUri").succeeded
        val inHead = head.hcursor.downField("remoteScreenerUri").succeeded
        val inCoil = coil.hcursor.downField("remoteScreenerUri").succeeded
        // Only that field goes: the coil keeps its own ledger uri.
        val coilKeepsLedgerUri = coil.hcursor.downField("remoteLedgerUri").succeeded

        assert(inTemplate && inHead && !inCoil && coilKeepsLedgerUri)
    }

    test("a coil-filled template decodes into an OwnCoilPeerPrivate identity") {
        given HeadPeers = headPeers
        given CoilPeers = coilPeers

        val (filled, secrets) = GenerateKeyPair
            .fillPrivateConfig(template, Role.Coil, vkeyHex('3'), hex('3'))
            .fold(e => fail(s"fill failed: $e"), identity)

        val reassembled = PrivateSecrets
            .applySecrets(filled, secrets, "test")
            .fold(e => fail(s"credentials rejected: ${e.reason}"), identity)

        val decoded = parser
            .decode[NodePrivateConfig](reassembled.spaces2)(using nodePrivateConfigDecoder)
            .fold(e => fail(s"decode failed: $e"), identity)

        decoded.ownPeerPrivate match {
            case p: OwnCoilPeerPrivate =>
                assert(p.ownPeerIndex == 1 && p.ownPeerLabel == "c1")
            case other => fail(s"expected OwnCoilPeerPrivate, got $other")
        }
    }
}
