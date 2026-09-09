package hydrozoa.multisig.ledger.block

import hydrozoa.config.ScriptReferenceUtxos
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.node.MultiNodeConfig
import io.circe.Json
import io.circe.syntax.*
import org.scalacheck.Gen
import org.scalacheck.rng.Seed
import org.scalatest.funsuite.AnyFunSuite

/** Block zero's header: every field follows from its creation end time, so nothing about it is
  * negotiable and nothing about it needs storing twice.
  *
  * `startTime` is derived rather than carried — block zero skips the fast cycle, so it has no
  * creation window — and `HeadConfig`'s decoder rebuilds the whole header to check the one the
  * config hands it.
  */
class BlockHeaderInitialTest extends AnyFunSuite:

    private val multiNodeConfig: MultiNodeConfig =
        MultiNodeConfig.generateDefault.pureApply(Gen.Parameters.default, Seed(0L))
    private val headConfig: HeadConfig = multiNodeConfig.headConfig
    private val header: BlockHeader.Initial = headConfig.initialBlock.blockBrief.header

    private given CardanoNetwork.Section = headConfig
    private given ScriptReferenceUtxos = headConfig.scriptReferenceUtxos

    test("block zero's start time is its end time") {
        assert(header.startTime.convert == header.endTime.convert)
    }

    test("the derived header is the one the config carries") {
        assert(BlockHeader.Initial.derive(header.endTime)(using headConfig.txTiming) == header)
    }

    test("the encoded header carries no startTime, and round-trips") {
        val encoded = header.asJson
        val _ = assert(encoded.hcursor.downField("startTime").failed)
        assert(encoded.as[BlockHeader.Initial] == Right(header))
    }

    /** Re-encode the head config with one field of block zero's header replaced. */
    private def configWithHeaderField(field: String, value: Json): Json =
        headConfig.asJson.hcursor
            .downField("blockBrief")
            .downField("header")
            .downField(field)
            .set(value)
            .top
            .getOrElse(fail(s"could not reach blockBrief.header.$field"))

    test("the config decodes as written") {
        assert(headConfig.asJson.as[HeadConfig].isRight)
    }

    test("a config written with a block-zero startTime still decodes; the field is ignored") {
        // Configs written before block zero's start time was derived carry a `startTime` that has
        // no relation to `endTime`. Nothing reads it now, so an existing head-config.json must
        // still decode — and the header it produces reports `startTime == endTime`.
        val stale = (header.endTime.convert.instant.toEpochMilli - 600_000L).asJson
        val withStartTime = headConfig.asJson.hcursor
            .downField("blockBrief")
            .downField("header")
            .withFocus(_.deepMerge(Json.obj("startTime" -> stale)))
            .top
            .getOrElse(fail("could not reach blockBrief.header"))
        withStartTime.as[HeadConfig] match
            case Left(e) => fail(s"a config carrying block zero's startTime was refused: $e")
            case Right(decoded) =>
                val h = decoded.initialBlock.blockBrief.header
                val _ = assert(h.startTime.convert == h.endTime.convert)
                assert(h == header)
    }

    test("a config whose block-zero header timings were edited is refused") {
        val moved = header.fallbackTxStartTime.convert.instant.toEpochMilli + 60_000L
        val tampered = configWithHeaderField("fallbackTxStartTime", moved.asJson)
        tampered.as[HeadConfig] match
            case Right(_) => fail("a hand-edited block-zero header decoded")
            case Left(e)  => assert(e.getMessage.contains("Block zero's header"))
    }

    test("a config whose block-zero deposit-decision wakeup was added is refused") {
        val added = header.endTime.convert.instant.toEpochMilli
        val tampered = configWithHeaderField("depositDecisionWakeupTime", added.asJson)
        tampered.as[HeadConfig] match
            case Right(_) => fail("a block-zero header with a deposit-decision wakeup decoded")
            case Left(e)  => assert(e.getMessage.contains("Block zero's header"))
    }
