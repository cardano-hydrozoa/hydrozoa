package hydrozoa.multisig.persistence.codec

import com.google.protobuf.{ByteString as ProtoBytes, InvalidProtocolBufferException}
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.consensus.{UserRequest, UserRequestBody, UserRequestWithId}
import hydrozoa.multisig.ledger.event.{RequestId, RequestNumber}
import hydrozoa.request.request_record as proto
import scalus.uplc.builtin.ByteString

/** Byte codec for the Request lane's durable record — the protobuf encoding declared in
  * `proto/request_record.proto`.
  *
  * This is the one journal lane whose stored payload is **not** the circe wire form. The lane is
  * written on the admission hot path and read by a process in another language, so it gets a
  * canonical binary encoding: `bytes` fields carry the L1/L2 payloads verbatim instead of the
  * hex-in-JSON round trip, and the field numbers are a schema contract rather than a set of key
  * names that can drift between the two repos.
  *
  * Everything about the encoding — field order, varints, length delimiting, proto3 default
  * omission, tolerating a field written by a newer build — belongs to the code ScalaPB generates
  * from that `.proto` at compile time. What is written by hand here is the mapping between the
  * generated message and this codebase's own types, so the `.proto` is the single definition of the
  * format rather than a description of one maintained separately in Scala.
  *
  * The golden fixtures under `src/test/resources/golden/request-record/` pin exact bytes, so a
  * change to the format fails a test instead of silently rewriting something another repo parses.
  */
object RequestRecordCodec:

    /** Encode an assigned request to its protobuf byte form. */
    def encode(request: UserRequestWithId): Array[Byte] =
        val id: RequestId = request.requestId
        val body = request.request.body match
            case UserRequestBody.DepositRequestBody(l1Payload, l2Payload) =>
                proto.RequestRecord.Body.Deposit(
                  proto.DepositBody(protoBytes(l1Payload), protoBytes(l2Payload))
                )
            case UserRequestBody.TransactionRequestBody(l2Payload) =>
                proto.RequestRecord.Body.Transaction(proto.TransactionBody(protoBytes(l2Payload)))
        proto
            .RequestRecord(
              headPeerNumber = id.peerNum,
              requestNumber = id.requestNum,
              body = body
            )
            .toByteArray

    /** Decode an assigned request from its protobuf byte form. Throws `IllegalArgumentException` on
      * a malformed or incomplete record — a value read back from the store or off the wire that
      * does not decode is corruption, and the persistence layer treats corruption as fail-fast.
      */
    def decode(encoded: Array[Byte]): UserRequestWithId =
        val message =
            try proto.RequestRecord.parseFrom(encoded)
            catch
                case e: InvalidProtocolBufferException =>
                    throw new IllegalArgumentException(
                      s"Request record is malformed: ${e.getMessage}",
                      e
                    )
        val request = message.body match
            case proto.RequestRecord.Body.Deposit(deposit) =>
                UserRequest.DepositRequest(
                  UserRequestBody.DepositRequestBody(
                    byteString(deposit.l1Payload),
                    byteString(deposit.l2Payload)
                  )
                )
            case proto.RequestRecord.Body.Transaction(transaction) =>
                UserRequest.TransactionRequest(
                  UserRequestBody.TransactionRequestBody(byteString(transaction.l2Payload))
                )
            case proto.RequestRecord.Body.Empty =>
                throw new IllegalArgumentException("Request record is missing its body")
        UserRequestWithId(
          userRequest = request,
          requestId = RequestId(
            HeadPeerNumber(message.headPeerNumber),
            RequestNumber(message.requestNumber)
          )
        )

    private def protoBytes(value: ByteString): ProtoBytes = ProtoBytes.copyFrom(value.bytes)

    private def byteString(value: ProtoBytes): ByteString = ByteString.fromArray(value.toByteArray)
