package hydrozoa.multisig.ledger.block

import hydrozoa.config.ScriptReferenceUtxos
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.head.multisig.timing.TxTiming
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.node.MultiNodeConfig
import io.circe.Json
import io.circe.syntax.*
import org.scalacheck.Gen
import org.scalacheck.rng.Seed
import org.scalatest.funsuite.AnyFunSuite

/** Block zero's header is its creation end time and nothing else: `startTime` repeats it, the two
  * wakeup times follow from it through [[TxTiming]], and there is no deposit-decision wakeup. So
  * `endTime` is all the header holds, all its JSON carries, and all a reader needs to rebuild it.
  */
class BlockHeaderInitialTest extends AnyFunSuite:

    private val multiNodeConfig: MultiNodeConfig =
        MultiNodeConfig.generateDefault.pureApply(Gen.Parameters.default, Seed(0L))
    private val headConfig: HeadConfig = multiNodeConfig.headConfig
    private val header: BlockHeader.Initial = headConfig.initialBlock.blockBrief.header

    private given CardanoNetwork.Section = headConfig
    private given ScriptReferenceUtxos = headConfig.scriptReferenceUtxos
    private given TxTiming = headConfig.txTiming

    test("block zero's start time is its end time") {
        assert(header.startTime.convert == header.endTime.convert)
    }

    test("block zero's wakeup times follow the head's tx timing") {
        val fallbackTxStartTime = headConfig.txTiming.newFallbackStartTime(header.endTime)
        val _ = assert(header.fallbackTxStartTime == fallbackTxStartTime)
        val _ = assert(
          header.forcedMajorBlockWakeupTime ==
              headConfig.txTiming.forcedMajorBlockWakeupTime(fallbackTxStartTime)
        )
        assert(header.mDepositDecisionWakeupTime.isEmpty)
    }

    test("the encoded header carries the end time alone, and round-trips") {
        val encoded = header.asJson
        val _ = assert(encoded.hcursor.keys.map(_.toList) == Some(List("endTime")))
        assert(encoded.as[BlockHeader.Initial] == Right(header))
    }

    test("the config decodes as written") {
        assert(headConfig.asJson.as[HeadConfig].isRight)
    }

    test("a config carrying block zero's derived header fields still decodes; they are ignored") {
        // Configs written before block zero's header collapsed to its end time carry the four
        // derived fields alongside it. Nothing reads them now, so an existing head-config.json
        // must still decode — and it must decode to the header `endTime` determines even when the
        // stored values disagree, which is what these deliberately wrong ones prove.
        val wrong = (header.endTime.convert.instant.toEpochMilli - 600_000L).asJson
        val withDerivedFields = headConfig.asJson.hcursor
            .downField("blockBrief")
            .downField("header")
            .withFocus(
              _.deepMerge(
                Json.obj(
                  "startTime" -> wrong,
                  "fallbackTxStartTime" -> wrong,
                  "forcedMajorBlockWakeupTime" -> wrong,
                  "depositDecisionWakeupTime" -> wrong
                )
              )
            )
            .top
            .getOrElse(fail("could not reach blockBrief.header"))
        withDerivedFields.as[HeadConfig] match
            case Left(e) => fail(s"a config carrying block zero's derived header fields: $e")
            case Right(decoded) =>
                val h = decoded.initialBlock.blockBrief.header
                val _ = assert(h.endTime == header.endTime)
                val _ = assert(h.startTime.convert == h.endTime.convert)
                val _ = assert(h.fallbackTxStartTime == header.fallbackTxStartTime)
                val _ = assert(h.forcedMajorBlockWakeupTime == header.forcedMajorBlockWakeupTime)
                assert(h.mDepositDecisionWakeupTime.isEmpty)
    }
