package hydrozoa.multisig.ledger.block

import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.{BlockCreationEndTime, BlockCreationStartTime, DepositDecisionWakeupTime, FallbackTxStartTime}
import hydrozoa.config.head.{HeadConfig, generateHeadConfig}
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.event.RequestId.ValidityFlag
import hydrozoa.multisig.ledger.event.{
  RequestHash,
  RequestId,
  RequestNumber
}
import org.scalacheck.Prop.propBoolean
import org.scalacheck.{Gen, Prop, Properties}
import scala.concurrent.duration.DurationInt
import scalus.cardano.ledger.{Blake2b_256, Hash}
import scalus.uplc.builtin.ByteString
import test.{TestPeers, TestPeersSpec}

/** [[BlockHash]] must be stable for a given block and must move when any covered field moves.
  *
  * A field that silently falls out of the preimage is invisible in production: two peers holding
  * blocks that differ in exactly that field agree on a digest that does not constrain it, and every
  * soft-ack verifies. So each covered field gets its own mutation here, and the block-type tag gets
  * the shape checks that keep the four block types from colliding.
  */
object BlockHashTest extends Properties("BlockHash") {

    override def overrideParameters(
        p: org.scalacheck.Test.Parameters
    ): org.scalacheck.Test.Parameters =
        p.withMinSuccessfulTests(20)

    /** One generated head config, used only for its `TxTiming` — `ForcedMajorBlockWakeupTime` has
      * no public constructor, so the one honest way to obtain one is to derive it as the protocol
      * does.
      */
    private val generateConfig =
        TestPeersSpec.generate().flatMap(TestPeers.generate).flatMap(generateHeadConfig().run(_))

    private def genRequestHash: Gen[RequestHash] =
        Gen.listOfN(32, Gen.choose(Byte.MinValue, Byte.MaxValue))
            .map(bytes =>
                RequestHash.fromHash(Hash[Blake2b_256, Any](ByteString.fromArray(bytes.toArray)))
            )

    private def genRequestId: Gen[RequestId] = for {
        peer <- Gen.choose(0, 10)
        num <- Gen.choose[Long](0, 1024)
    } yield RequestId(HeadPeerNumber(peer), RequestNumber(num))

    private def genRequest: Gen[(RequestId, RequestHash, ValidityFlag)] = for {
        id <- genRequestId
        hash <- genRequestHash
        validity <- Gen.oneOf(ValidityFlag.Valid, ValidityFlag.Invalid)
    } yield (id, hash, validity)

    /** A minor header and its body, built the way the protocol builds them: block zero's header
      * supplies the forward times, and the block's own numbering and window are generated.
      */
    private def genMinor(hc: HeadConfig): Gen[BlockBrief.Minor] = for {
        blockNum <- Gen.choose(1, 1000).map(BlockNumber.apply)
        versionMajor <- Gen.choose(0, 50)
        versionMinor <- Gen.choose(0, 50)
        windowSeconds <- Gen.choose(1, 60)
        requests <- Gen.nonEmptyListOf(genRequest)
        rejected <- Gen.listOf(genRequestId)
        wakeup <- Gen.option(Gen.choose(1, 600))
    } yield {
        val zero = hc.initialBlock.blockBrief.header
        val startTime = zero.endTime.convert
        val header = BlockHeader.Minor(
          blockNum = blockNum,
          blockVersion = BlockVersion.Full(versionMajor, versionMinor),
          startTime = BlockCreationStartTime(startTime),
          endTime = BlockCreationEndTime(startTime + windowSeconds.seconds),
          fallbackTxStartTime = zero.fallbackTxStartTime,
          forcedMajorBlockWakeupTime = zero.forcedMajorBlockWakeupTime,
          mDepositDecisionWakeupTime =
              wakeup.map(s => DepositDecisionWakeupTime(startTime + s.seconds))
        )
        BlockBrief.Minor(header, BlockBody.Minor(requests, rejected))
    }

    private val generateBrief: Gen[(HeadConfig, BlockBrief.Minor)] =
        generateConfig.flatMap(hc => genMinor(hc).map(hc -> _))

    val _ = property("is deterministic") = Prop.forAll(generateBrief) { (_, brief) =>
        BlockHash(brief.header, brief.body) == BlockHash(brief.header, brief.body)
    }

    val _ = property("a brief derives its own digest") = Prop.forAll(generateBrief) { (_, brief) =>
        brief.blockHash == BlockHash(brief.header, brief.body)
    }

    val _ = property("covers blockNum") = Prop.forAll(generateBrief) { (_, brief) =>
        val moved = brief.header.copy(blockNum = brief.header.blockNum.increment)
        BlockHash(moved, brief.body) != brief.blockHash
    }

    val _ = property("covers versionMajor") = Prop.forAll(generateBrief) { (_, brief) =>
        val moved = brief.header.copy(blockVersion = brief.header.blockVersion.incrementMajor)
        BlockHash(moved, brief.body) != brief.blockHash
    }

    val _ = property("covers versionMinor") = Prop.forAll(generateBrief) { (_, brief) =>
        val moved = brief.header.copy(blockVersion = brief.header.blockVersion.incrementMinor)
        BlockHash(moved, brief.body) != brief.blockHash
    }

    val _ = property("covers startTime") = Prop.forAll(generateBrief) { (_, brief) =>
        val moved = brief.header.copy(startTime =
            BlockCreationStartTime(brief.header.startTime.convert + 1.second)
        )
        BlockHash(moved, brief.body) != brief.blockHash
    }

    val _ = property("covers endTime") = Prop.forAll(generateBrief) { (_, brief) =>
        val moved =
            brief.header.copy(endTime =
                BlockCreationEndTime(brief.header.endTime.convert + 1.second)
            )
        BlockHash(moved, brief.body) != brief.blockHash
    }

    val _ = property("covers fallbackTxStartTime") = Prop.forAll(generateBrief) { (_, brief) =>
        val moved = brief.header.copy(fallbackTxStartTime =
            FallbackTxStartTime(brief.header.fallbackTxStartTime.convert + 1.second)
        )
        BlockHash(moved, brief.body) != brief.blockHash
    }

    /** `None` and a present value must not produce the same bytes, which is what the
      * flag-then-value layout is for.
      */
    val _ = property("covers mDepositDecisionWakeupTime") = Prop.forAll(generateBrief) {
        (_, brief) =>
            val moved = brief.header.copy(mDepositDecisionWakeupTime =
                brief.header.mDepositDecisionWakeupTime match {
                    case None    => Some(DepositDecisionWakeupTime(brief.header.endTime.convert))
                    case Some(_) => None
                }
            )
            BlockHash(moved, brief.body) != brief.blockHash
    }

    /** The point of the whole digest: change which payload sits at a position, keeping the position
      * itself, and the block's digest moves. This is what a `RequestId` alone cannot express.
      */
    val _ = property("covers each request's hash") = Prop.forAll(generateBrief, genRequestHash) {
        case ((_, brief), other) =>
            val (id, hash, validity) = brief.body.requests.head
            (hash != other) ==> {
                val moved =
                    brief.body.copy(requests = (id, other, validity) :: brief.body.requests.tail)
                BlockHash(brief.header, moved) != brief.blockHash
            }
    }

    val _ = property("covers each request's id") = Prop.forAll(generateBrief) { (_, brief) =>
        val (id, hash, validity) = brief.body.requests.head
        val moved =
            brief.body.copy(requests = (id.increment, hash, validity) :: brief.body.requests.tail)
        BlockHash(brief.header, moved) != brief.blockHash
    }

    val _ = property("covers each request's validity") = Prop.forAll(generateBrief) { (_, brief) =>
        val (id, hash, validity) = brief.body.requests.head
        val flipped =
            if validity == ValidityFlag.Valid then ValidityFlag.Invalid else ValidityFlag.Valid
        val moved = brief.body.copy(requests = (id, hash, flipped) :: brief.body.requests.tail)
        BlockHash(brief.header, moved) != brief.blockHash
    }

    /** Order is the leader's choice, so a reordering is a disagreement about block content and must
      * not hash the same.
      */
    val _ = property("covers request order") = Prop.forAll(generateBrief) { (_, brief) =>
        val requests = brief.body.requests
        (requests.size >= 2 && requests.head != requests(1)) ==> {
            val swapped = requests(1) :: requests.head :: requests.drop(2)
            BlockHash(brief.header, brief.body.copy(requests = swapped)) != brief.blockHash
        }
    }

    val _ = property("covers depositsRejected") = Prop.forAll(generateBrief, genRequestId) {
        case ((_, brief), extra) =>
            val moved = brief.body.copy(depositsRejected = extra :: brief.body.depositsRejected)
            BlockHash(brief.header, moved) != brief.blockHash
    }

    /** A major block's absorption decisions are the leader's, and a follower has to be told them,
      * so they are part of what the signature set attests to.
      */
    val _ = property("covers depositsAbsorbed") = Prop.forAll(generateBrief, genRequestId) {
        case ((_, brief), absorbed) =>
            val header = BlockHeader.Major(
              blockNum = brief.header.blockNum,
              blockVersion = brief.header.blockVersion,
              startTime = brief.header.startTime,
              endTime = brief.header.endTime,
              fallbackTxStartTime = brief.header.fallbackTxStartTime,
              forcedMajorBlockWakeupTime = brief.header.forcedMajorBlockWakeupTime,
              mDepositDecisionWakeupTime = brief.header.mDepositDecisionWakeupTime
            )
            val body = BlockBody.Major(
              brief.body.requests,
              List.empty,
              brief.body.depositsRejected
            )
            BlockHash(header, body) != BlockHash(
              header,
              body.copy(depositsAbsorbed = List(absorbed))
            )
    }

    /** The block-type tag is what keeps the four shapes apart. A minor and a major that agree on
      * every field a minor has must still hash differently.
      */
    val _ = property("separates minor from major") = Prop.forAll(generateBrief) { (_, brief) =>
        val header = BlockHeader.Major(
          blockNum = brief.header.blockNum,
          blockVersion = brief.header.blockVersion,
          startTime = brief.header.startTime,
          endTime = brief.header.endTime,
          fallbackTxStartTime = brief.header.fallbackTxStartTime,
          forcedMajorBlockWakeupTime = brief.header.forcedMajorBlockWakeupTime,
          mDepositDecisionWakeupTime = brief.header.mDepositDecisionWakeupTime
        )
        val body =
            BlockBody.Major(brief.body.requests, List.empty, brief.body.depositsRejected)
        BlockHash(header, body) != brief.blockHash
    }

    val _ = property("separates minor from final") = Prop.forAll(generateBrief) { (_, brief) =>
        val header = BlockHeader.Final(
          blockNum = brief.header.blockNum,
          blockVersion = brief.header.blockVersion,
          startTime = brief.header.startTime,
          endTime = brief.header.endTime
        )
        val body = BlockBody.Final(brief.body.requests, brief.body.depositsRejected)
        BlockHash(header, body) != brief.blockHash
    }

    /** Block zero's digest proves nothing new — its header is already pinned by `headParamsHash`
      * through the initialization transaction — but it has to exist, so that `blockHash` is total
      * on `BlockBrief` and no consumer needs a special case.
      */
    val _ = property("the initial block has a digest") = Prop.forAll(generateConfig) { hc =>
        val initial = hc.initialBlock.blockBrief
        initial.blockHash == BlockHash(initial.header, BlockBody.Initial)
    }
}
