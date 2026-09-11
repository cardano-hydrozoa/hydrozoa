package hydrozoa.multisig.ledger.joint

import hydrozoa.multisig.consensus.peer.{CoilPeerNumber, HeadPeerNumber, PeerId}
import hydrozoa.multisig.consensus.{UserRequest, UserRequestBody, UserRequestWithId}
import hydrozoa.multisig.ledger.event.{RequestId, RequestNumber}
import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.ByteString

/** [[JointLedger.mkHashOf]] picks which digest a block is built from: the one this peer verified,
  * for its own requests, and one recomputed from the body, for everyone else's.
  *
  * Every request here carries a digest that does **not** describe its body, so the digest that
  * comes back says which path was taken. A request built honestly would pass either way and prove
  * nothing.
  */
class MkHashOfTest extends AnyFunSuite {

    private val body: UserRequestBody.TransactionRequestBody =
        UserRequestBody.TransactionRequestBody(ByteString.fromHex("cafe00"))

    /** A digest of some other body — well-formed, and wrong for [[body]]. */
    private val carried =
        UserRequestBody.TransactionRequestBody(ByteString.fromHex("beef")).mkHash

    private def requestFrom(author: Int): UserRequestWithId =
        UserRequestWithId(
          UserRequest.TransactionRequest(body, carried),
          RequestId(HeadPeerNumber(author), RequestNumber(7))
        )

    test("an own request reuses the digest its sequencer verified, without hashing again") {
        assert(JointLedger.mkHashOf(PeerId.Head(HeadPeerNumber(2)), requestFrom(2)) == carried)
    }

    test("an alien request's carried digest is ignored and recomputed from the body") {
        assert(JointLedger.mkHashOf(PeerId.Head(HeadPeerNumber(1)), requestFrom(2)) == body.mkHash)
    }

    /** A coil peer assigns nothing, so it never trusts a carried digest — not even one whose id
      * happens to share a number with the coil's own.
      */
    test("every request is alien to a coil peer") {
        assert(
          JointLedger.mkHashOf(PeerId.Coil(CoilPeerNumber(2)), requestFrom(2)) == body.mkHash
        )
    }
}
