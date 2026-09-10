package hydrozoa.multisig.consensus

import hydrozoa.multisig.consensus.UserRequestBody.{DepositRequestBody, TransactionRequestBody}
import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.Builtins.blake2b_256
import scalus.uplc.builtin.ByteString

/** [[UserRequestBody.hash]] is a public interface: a submitter has to reproduce it to get a request
  * accepted, so these vectors are the contract. They are the worked examples in
  * `docs/user-guide/REQUEST-HASH.md`, and moving one is a client break.
  */
class UserRequestTest extends AnyFunSuite {

    private val l1Payload = ByteString.fromHex(
      "84a600d901028182582033e69278d5ead6e2cd95a5858aa1bb1044de857cc179f6fdedbbb30168af5db5020182a300581d70d7eca5332819b0a79a02741d4e3fd145b779cc266e00c970135e82d4011a00989680028201d818583dd8799fd8799fd8799fd8799f581ccca3ccca722cf0fa459a1d34817b555cae8375859803800a3b61f1caffd87a80ffd87a801b0000019d8f830a58ffff82581d60cca3ccca722cf0fa459a1d34817b555cae8375859803800a3b61f1ca821b0000000119912e24a1581c45df5f274b8950b512b08d10656864958659c4ecf3ffad092ef63024a14455534472191890021a0002cc65031a0687bafb07582027bea310983e134e70f658559c13266f903f3a46920009b1c87f883255a953fa12d901028182582033e69278d5ead6e2cd95a5858aa1bb1044de857cc179f6fdedbbb30168af5db501a0f5a1191349a1674465706f736974a1784030313334393930306163633030666136396534653066306334626661623362643534653564333639653232393834346332616438313137646535616135363637a3696465706f7369744978006a6465706f736974466565006d6c325061796c6f616448617368784030633064373064333862323938373537323231393935323363303832383538323738633661323739306661656532396236646236623136623036383535373930"
    )

    private val l2Payload = ByteString.fromHex(
      "7b226163636f756e744964223a226363613363636361373232636630666134353961316433343831376235353563616538333735383539383033383030613362363166316361222c2264656c6567617465644b6579223a223537633861663235366135643632616238643933396466613232613264373933623037623034306538613831656638633963333861333932227d"
    )

    test("User request body hashes as expected (deposits)") {
        val body = DepositRequestBody(l1Payload = l1Payload, l2Payload = l2Payload)
        assert(
          body.hash.toHex == "ac596c7fb689a6e4757fb6c782580d7cc2ca0ff812ae872753be1292063a28bb"
        )
    }

    test("User request body hashes as expected (txs)") {
        val body = TransactionRequestBody(l2Payload = l2Payload)
        assert(
          body.hash.toHex == "58828159aaac6c4575395db0ea87f5e2a378c2e3f4c6e78b27d61eddbd2b1e85"
        )
    }

    /** The variant tag is the only thing separating these two: a transaction whose `l2Payload` is
      * exactly a deposit's pair of payload digests feeds the same trailing bytes into the preimage.
      * Without the tag the two hash identically, and one request kind could be passed off as the
      * other.
      */
    test("A transaction cannot collide with the deposit whose digests it carries") {
        val deposit = DepositRequestBody(l1Payload = l1Payload, l2Payload = l2Payload)
        val impostor = TransactionRequestBody(
          l2Payload = blake2b_256(l1Payload).concat(blake2b_256(l2Payload))
        )
        assert(deposit.hash != impostor.hash)
    }

    /** The digest describes the body and nothing around it, which is what lets a submitter compute
      * it before the head has assigned a `RequestId` — and lets two peers that received the same
      * request agree on its digest without agreeing on anything else.
      */
    test("Equal bodies hash equally") {
        val _ = assert(
          TransactionRequestBody(l2Payload).hash == TransactionRequestBody(l2Payload).hash
        )
        assert(
          DepositRequestBody(l1Payload, l2Payload).hash
              == DepositRequestBody(l1Payload, l2Payload).hash
        )
    }

    /** Both payloads reach the preimage, each through its own digest. Swapping them is the case the
      * two-digest rule exists for: hashing the raw concatenation instead would collapse
      * `hash(abc + def)` and `hash(ab + cdef)`.
      */
    test("A deposit's two payloads are not interchangeable") {
        assert(
          DepositRequestBody(l1Payload, l2Payload).hash
              != DepositRequestBody(l2Payload, l1Payload).hash
        )
    }

    /** The gate `RequestSequencer` runs before it assigns a `RequestId`. */
    test("A request built from its own body passes the head's check") {
        val _ = assert(
          UserRequest.TransactionRequest(TransactionRequestBody(l2Payload)).checkRequestHash
              == Right(())
        )
        assert(
          UserRequest.DepositRequest(DepositRequestBody(l1Payload, l2Payload)).checkRequestHash
              == Right(())
        )
    }

    test("A request whose digest describes other bytes is refused, naming both digests") {
        val body: TransactionRequestBody = TransactionRequestBody(l2Payload)
        val wrong = DepositRequestBody(l1Payload, l2Payload).hash
        val refused = UserRequest.TransactionRequest(body, wrong).checkRequestHash
        val _ = assert(refused.isLeft)
        val _ = assert(refused.left.exists(_.contains(wrong.toHex)))
        assert(refused.left.exists(_.contains(body.hash.toHex)))
    }

    /** The failure the end-to-end check exists for: the head holds fewer bytes than the submitter
      * hashed, and nothing else in the request would notice.
      */
    test("A truncated payload is refused") {
        val whole: TransactionRequestBody = TransactionRequestBody(l2Payload)
        val truncated: TransactionRequestBody =
            TransactionRequestBody(ByteString.fromArray(l2Payload.bytes.dropRight(1)))
        assert(UserRequest.TransactionRequest(truncated, whole.hash).checkRequestHash.isLeft)
    }
}
