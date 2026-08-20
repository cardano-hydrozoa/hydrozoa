package hydrozoa.multisig.persistence.codec

import com.google.protobuf.ByteString as ProtoBytes
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.consensus.{UserRequest, UserRequestBody, UserRequestWithId}
import hydrozoa.multisig.ledger.event.{RequestId, RequestNumber}
import hydrozoa.request.request_record as proto
import java.nio.file.{Files, Path, Paths}
import org.scalatest.funsuite.AnyFunSuite
import scala.jdk.CollectionConverters.*
import scalus.uplc.builtin.ByteString

/** Tests for the Request lane's canonical protobuf record.
  *
  * The **golden fixtures** are the point: each case's exact encoded bytes are committed under
  * `src/test/resources/golden/request-record/`, so a change to the encoding fails here rather than
  * silently rewriting a store format that a reader in another repo also parses. Regenerate them
  * deliberately with `HZ_UPDATE_GOLDEN=1 sbt "testOnly *RequestRecordCodecTest"` — and treat a
  * regeneration as a store-format change (bump `StoreVersion.current`, revendor the fixtures).
  */
class RequestRecordCodecTest extends AnyFunSuite:

    /** Fixtures are read from the source tree, not the classpath, so a regeneration run updates the
      * committed files rather than a build output nobody reviews.
      *
      * Declared before anything that reads them: Scala 3's initialization checker cannot prove a
      * `val` defined lower down is set by the time a `test(...)` body registered during
      * construction captures it, and under CI's `-Werror` that warning is a build failure.
      */
    private val goldenDir: Path =
        Paths.get("src", "test", "resources", "golden", "request-record")

    private val updateGolden: Boolean = sys.env.get("HZ_UPDATE_GOLDEN").contains("1")

    /** The fixture cases, chosen to cover the encoding's edges: proto3 default omission (peer 0 /
      * request 0 encode as absent fields), both body arms, a request number past 2^32 (multi-byte
      * varint), and a realistic transaction payload.
      */
    private val cases: List[(String, UserRequestWithId)] = List(
      "transaction-zero-id" -> transaction(0, 0L, payload(4)),
      "transaction-large-number" -> transaction(5, (1L << 32) + 12345L, payload(16)),
      "transaction-realistic" -> transaction(2, 913L, payload(800)),
      "deposit-basic" -> deposit(3, 7L, payload(220), payload(96))
    )

    test("every fixture round-trips through encode / decode") {
        cases.foreach { (name, record) =>
            val decoded = RequestRecordCodec.decode(RequestRecordCodec.encode(record))
            assert(decoded == record, s"round trip changed $name")
        }
    }

    test("every fixture encodes to its committed golden bytes") {
        cases.foreach { (name, record) =>
            val encoded = RequestRecordCodec.encode(record)
            if updateGolden then writeGolden(name, encoded)
            assert(
              toHex(encoded) == toHex(readGolden(name)),
              s"encoding of $name changed; if that was deliberate, regenerate the fixtures"
            )
        }
    }

    test("every golden fixture decodes to its case") {
        cases.foreach { (name, record) =>
            assert(RequestRecordCodec.decode(readGolden(name)) == record, s"decoding $name")
        }
    }

    test("a field written by a newer build is skipped, not misread") {
        val (_, record) = cases.head
        val forwards = proto.RequestRecord
            .parseFrom(RequestRecordCodec.encode(record))
            .withUnknownFields(
              scalapb
                  .UnknownFieldSet()
                  .withField(
                    9,
                    scalapb.UnknownFieldSet.Field(lengthDelimited =
                        Seq(ProtoBytes.copyFromUtf8("a field this build does not know"))
                    )
                  )
            )
            .toByteArray
        assert(RequestRecordCodec.decode(forwards) == record)
    }

    test("a record with no body is rejected") {
        val bytes = proto.RequestRecord(headPeerNumber = 3, requestNumber = 7L).toByteArray
        val failure = intercept[IllegalArgumentException](RequestRecordCodec.decode(bytes))
        assert(failure.getMessage.contains("body"))
    }

    private def transaction(peer: Int, num: Long, l2Payload: Array[Byte]): UserRequestWithId =
        UserRequestWithId(
          UserRequest.TransactionRequest(
            UserRequestBody.TransactionRequestBody(ByteString.fromArray(l2Payload))
          ),
          RequestId(HeadPeerNumber(peer), RequestNumber(num))
        )

    private def deposit(
        peer: Int,
        num: Long,
        l1Payload: Array[Byte],
        l2Payload: Array[Byte]
    ): UserRequestWithId =
        UserRequestWithId(
          UserRequest.DepositRequest(
            UserRequestBody.DepositRequestBody(
              ByteString.fromArray(l1Payload),
              ByteString.fromArray(l2Payload)
            )
          ),
          RequestId(HeadPeerNumber(peer), RequestNumber(num))
        )

    /** A deterministic byte payload of the given length — fixtures must not vary between runs. */
    private def payload(length: Int): Array[Byte] =
        Array.tabulate(length)(i => (i * 37 + 11).toByte)

    private def readGolden(name: String): Array[Byte] =
        val path = goldenDir.resolve(s"$name.hex")
        if !Files.exists(path) then
            fail(s"missing golden fixture $path; regenerate with HZ_UPDATE_GOLDEN=1")
        fromHex(Files.readString(path).trim)

    private def writeGolden(name: String, bytes: Array[Byte]): Unit =
        Files.createDirectories(goldenDir): Unit
        Files.write(goldenDir.resolve(s"$name.hex"), List(toHex(bytes)).asJava): Unit

    private def toHex(bytes: Array[Byte]): String =
        bytes.map(b => f"${b & 0xff}%02x").mkString

    private def fromHex(hex: String): Array[Byte] =
        hex.grouped(2).map(pair => Integer.parseInt(pair, 16).toByte).toArray
