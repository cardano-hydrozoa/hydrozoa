package hydrozoa.multisig.consensus.transport

import hydrozoa.lib.crypto.Preimage
import hydrozoa.multisig.consensus.peer.PeerWallet
import java.nio.charset.StandardCharsets.UTF_8
import scala.util.control.NonFatal
import scalus.cardano.ledger.Hash32
import scalus.crypto.ed25519.VerificationKey
import scalus.uplc.builtin.{ByteString, platform}

/** The message a dialer signs to prove which peer it is, and the check the accept side runs on it.
  *
  * Every peer already holds an Ed25519 signing key and every roster already enumerates the
  * counterpart verification keys, so the credential a liaison link authenticates with is the one
  * the protocol is already built on — not a second identity registry.
  *
  * ```
  * handshakeProof = "gummiworm-handshake-v1"
  *   || <link>            // one byte: which link this claim is good for
  *   || <claimant>        // the coil or head peer number being claimed
  *   || <protocolVersion>
  *   || <headParamsHash>
  *   || <nonce>           // the server's per-socket challenge
  * ```
  *
  * Every term is load-bearing:
  *
  *   - **`link`** keeps a proof harvested on one lane worthless on the other. Without it a hub's
  *     `/head` handshake is a valid-looking `/hub` one for the same number.
  *   - **`headParamsHash`** refuses both a peer from another head and a peer that disagrees about
  *     this one, at connect rather than on divergence. It subsumes the head id — it is one digest
  *     over the whole head config, so it also covers the rosters, the timings and the script
  *     references — and it is already computed and already in the multisig regime datum, so it
  *     costs nothing to send.
  *   - **`nonce`** binds the signature to this socket ([[HandshakeNonce]]).
  */
object HandshakeProof {

    /** Which liaison link a proof is good for. One byte in the preimage, so a signature made for
      * one link cannot be replayed on the other.
      */
    enum Link(val tag: Int) {

        /** The coil→hub uplink — the `/hub` route, [[CoilFrame]] envelope. The claimant is a
          * [[hydrozoa.multisig.consensus.peer.CoilPeerNumber]].
          */
        case CoilToHub extends Link(0x01)

        /** The head mesh — the `/head` route, [[HeadFrame]] envelope. The claimant is a
          * [[hydrozoa.multisig.consensus.peer.HeadPeerNumber]].
          */
        case HeadToHead extends Link(0x02)
    }

    /** Mixed in first so this signature can never be mistaken for a signature of the same bytes
      * taken for another purpose — a hard ack's tx body hash, say. ASCII, no terminator: the
      * fixed-width field that follows makes the boundary unambiguous.
      */
    val domainTag: Array[Byte] = "gummiworm-handshake-v1".getBytes(UTF_8)

    /** The bytes both ends compute. Written out field by field for the reason
      * [[hydrozoa.lib.crypto.Preimage]] gives: an encoder tweak that silently moved this value
      * would lock a fleet out of its own head.
      */
    def preimage(
        link: Link,
        claimant: Int,
        protocolVersion: Int,
        headParamsHash: Hash32,
        nonce: HandshakeNonce
    ): Array[Byte] = {
        val out = Preimage()
        out.raw(domainTag)
        out.u8(link.tag)
        out.u32(claimant)
        out.u32(protocolVersion)
        out.hash32(headParamsHash)
        out.raw(nonce.bytes.toArray)
        out.bytes
    }

    /** Sign the proof this node offers on `link`, answering the server's `nonce`. */
    def sign(
        wallet: PeerWallet,
        link: Link,
        claimant: Int,
        protocolVersion: Int,
        headParamsHash: Hash32,
        nonce: HandshakeNonce
    ): HandshakeAuth =
        HandshakeAuth(
          headParamsHash,
          HandshakeSignature(
            wallet.signMsg(
              IArray.from(preimage(link, claimant, protocolVersion, headParamsHash, nonce))
            )
          )
        )

    /** Check an inbound handshake's proof against the roster key the claimed number resolves to.
      *
      * Runs after the caller has checked the protocol version and admitted the claimed number — the
      * number is what resolves `vkey`, so there is nothing to verify until it does. Returns the
      * refusal to send back, or `Right(())` for a proven claim.
      */
    def verify(
        vkey: VerificationKey,
        link: Link,
        claimant: Int,
        protocolVersion: Int,
        ownHeadParamsHash: Hash32,
        nonce: HandshakeNonce,
        auth: HandshakeAuth
    ): Either[HandshakeRefusal, Unit] =
        // The head params are compared before the signature so a peer on another head is told which
        // of the two it is. A forged hash only ever fails the signature below, because the preimage
        // carries it.
        if auth.headParamsHash != ownHeadParamsHash then
            Left(HandshakeRefusal.HeadParamsMismatch(auth.headParamsHash, ownHeadParamsHash))
        else
            val message = preimage(link, claimant, protocolVersion, auth.headParamsHash, nonce)
            Either.cond(
              verifyEd25519(vkey, message, auth.signature),
              (),
              HandshakeRefusal.BadSignature
            )

    /** `platform.verifyEd25519Signature` throws on a malformed key or signature rather than
      * returning false, and a peer supplies both — so a length nobody checked must read as a
      * refusal, not as a transport crash. Mirrors
      * [[hydrozoa.multisig.consensus.HardAckSignatureVerifier]].
      */
    private def verifyEd25519(
        vkey: VerificationKey,
        message: Array[Byte],
        signature: HandshakeSignature
    ): Boolean =
        try
            platform.verifyEd25519Signature(
              vkey,
              ByteString.fromArray(message),
              ByteString.fromArray(signature.bytes)
            )
        catch { case NonFatal(_) => false }
}
