package hydrozoa.multisig.consensus.transport

import hydrozoa.multisig.consensus.peer.{CoilPeerNumber, HeadPeerNumber}
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.Hash32
import scalus.crypto.ed25519.VerificationKey

/** Every term of [[HandshakeProof.preimage]] must be load-bearing: change one and the proof stops
  * verifying. One test per term, because a term that is silently not covered is a replay the whole
  * exchange was built to stop.
  */
class HandshakeProofTest extends AnyFunSuite {

    private val coil1: VerificationKey =
        HandshakeFixture.coilPeers.verificationKey(CoilPeerNumber(1)).get

    private val head0: VerificationKey =
        HandshakeFixture.headPeers.headPeerVKey(HeadPeerNumber(0)).get

    /** Coil peer 1's honest proof: its own key, its own number, the current version, this head's
      * params, and the nonce it was challenged with.
      */
    private def honestCoilAuth(
        claimant: Int = 1,
        headParamsHash: Hash32 = HandshakeFixture.headParamsHash,
        nonce: HandshakeNonce = HandshakeFixture.nonce,
        link: HandshakeProof.Link = HandshakeProof.Link.CoilToHub,
        protocolVersion: Int = ProtocolVersion.current
    ): HandshakeAuth =
        HandshakeProof.sign(
          HandshakeFixture.coilWallet(1),
          link,
          claimant,
          protocolVersion,
          headParamsHash,
          nonce
        )

    /** Check an auth the way the hub does: against coil 1's roster key, on the coil link, for
      * claimant 1, at the current version, with this head's params and the fixture nonce.
      */
    private def check(
        auth: HandshakeAuth,
        vkey: VerificationKey = coil1,
        link: HandshakeProof.Link = HandshakeProof.Link.CoilToHub,
        claimant: Int = 1,
        protocolVersion: Int = ProtocolVersion.current,
        ownHeadParamsHash: Hash32 = HandshakeFixture.headParamsHash,
        nonce: HandshakeNonce = HandshakeFixture.nonce
    ): Either[HandshakeRefusal, Unit] =
        HandshakeProof.verify(
          vkey,
          link,
          claimant,
          protocolVersion,
          ownHeadParamsHash,
          nonce,
          auth
        )

    test("an honest proof verifies") {
        assert(check(honestCoilAuth()) == Right(()))
    }

    test("a proof offered with no signature is unauthenticated, not a bad signature") {
        // The two are different operator actions: an old build versus an impersonator.
        assert(
          check(HandshakeAuth.Unauthenticated) == Left(HandshakeRefusal.Unauthenticated)
        )
    }

    test("head params are compared before the signature, so the mismatch is what gets named") {
        assert(
          check(honestCoilAuth(headParamsHash = HandshakeFixture.otherHeadParamsHash)) ==
              Left(
                HandshakeRefusal.HeadParamsMismatch(
                  HandshakeFixture.otherHeadParamsHash,
                  HandshakeFixture.headParamsHash
                )
              )
        )
    }

    test("a proof made for another nonce does not verify — the replay the challenge stops") {
        assert(
          check(honestCoilAuth(nonce = HandshakeFixture.otherNonce)) ==
              Left(HandshakeRefusal.BadSignature)
        )
    }

    test("a proof made for the other link does not verify — cross-lane replay") {
        assert(
          check(honestCoilAuth(link = HandshakeProof.Link.HeadToHead)) ==
              Left(HandshakeRefusal.BadSignature)
        )
    }

    test("a proof made for another peer number does not verify") {
        // Coil 1's own key, over coil 0's number — checked as coil 1, which is what the hub does
        // once it has resolved the claimed number to a roster key.
        assert(check(honestCoilAuth(claimant = 0)) == Left(HandshakeRefusal.BadSignature))
    }

    test("a proof made at another protocol version does not verify") {
        assert(
          check(honestCoilAuth(protocolVersion = ProtocolVersion.current + 1)) ==
              Left(HandshakeRefusal.BadSignature)
        )
    }

    test("coil 1's proof does not verify under another peer's key") {
        assert(check(honestCoilAuth(), vkey = head0) == Left(HandshakeRefusal.BadSignature))
    }

    test("a stranger's proof for coil 1 does not verify under coil 1's key") {
        val forged = HandshakeProof.sign(
          HandshakeFixture.strangerWallet,
          HandshakeProof.Link.CoilToHub,
          claimant = 1,
          ProtocolVersion.current,
          HandshakeFixture.headParamsHash,
          HandshakeFixture.nonce
        )
        assert(check(forged) == Left(HandshakeRefusal.BadSignature))
    }

    test("a malformed signature is a refusal, not a thrown exception") {
        // `platform.verifyEd25519Signature` throws on a wrong-length signature, and a peer supplies
        // that length — so a transport that let it through would die on a frame instead of
        // refusing one.
        val garbage = HandshakeAuth.Signed(
          HandshakeFixture.headParamsHash,
          HandshakeSignature(IArray.from(Array.fill(7)(0x00.toByte)))
        )
        assert(check(garbage) == Left(HandshakeRefusal.BadSignature))
    }

    test("the preimage separates the two links") {
        val coil = HandshakeProof.preimage(
          HandshakeProof.Link.CoilToHub,
          claimant = 1,
          ProtocolVersion.current,
          HandshakeFixture.headParamsHash,
          HandshakeFixture.nonce
        )
        val mesh = HandshakeProof.preimage(
          HandshakeProof.Link.HeadToHead,
          claimant = 1,
          ProtocolVersion.current,
          HandshakeFixture.headParamsHash,
          HandshakeFixture.nonce
        )
        assert(!coil.sameElements(mesh))
    }
}
