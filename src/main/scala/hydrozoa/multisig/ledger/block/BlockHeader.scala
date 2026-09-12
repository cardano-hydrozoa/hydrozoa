package hydrozoa.multisig.ledger.block

import cats.Monad
import cats.implicits.*
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.given
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.{BlockCreationEndTime, BlockCreationStartTime, DepositDecisionWakeupTime, FallbackTxStartTime, ForcedMajorBlockWakeupTime}
import hydrozoa.config.head.multisig.timing.TxTiming.RequestTimes.DepositAbsorptionStartTime
import hydrozoa.config.head.multisig.timing.{TxTiming, TxTimingEvent}
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.lib.logging.ContraTracer
import io.circe.*
import io.circe.generic.semiauto.*
import io.circe.syntax.*
import java.time.Instant

sealed trait BlockHeader extends BlockHeader.Section {
    def asUnsigned: this.type & BlockStatus.Unsigned =
        this.asInstanceOf[this.type & BlockStatus.Unsigned]
    def asHardConfirmed: this.type & BlockStatus.HardConfirmed =
        this.asInstanceOf[this.type & BlockStatus.HardConfirmed]
}

object BlockHeader {

    /** Block zero's header, fixed in full by its creation end time.
      *
      * The initialization transaction pins `endTime` through its validity end and the rest follows:
      * `fallbackTxStartTime` and `forcedMajorBlockWakeupTime` through [[TxTiming]], `startTime`
      * because block zero has no creation window, and `mDepositDecisionWakeupTime` because block
      * zero absorbs no deposits. `endTime` is the whole representation — it is all the header holds
      * and all its JSON carries.
      *
      * @param endTime
      *   creation end time: when the moderator (head peer 0) received all the information to create
      *   the head config and broadcast it to the peers.
      */
    final case class Initial(
        override val endTime: BlockCreationEndTime
    )(using txTiming: TxTiming)
        extends BlockHeader,
          BlockType.Initial,
          NonFinal.Section {
        override transparent inline def blockNum: BlockNumber = Initial.blockNum
        override transparent inline def blockVersion: BlockVersion.Full = Initial.blockVersion
        override transparent inline def header: BlockHeader.Initial = this

        /** Block zero has no creation window: it skips the fast cycle entirely, so there is no
          * moment at which its weaving started. The start time is the end time.
          */
        override def startTime: BlockCreationStartTime = BlockCreationStartTime(endTime.convert)

        override val fallbackTxStartTime: FallbackTxStartTime =
            txTiming.newFallbackStartTime(endTime)

        override val forcedMajorBlockWakeupTime: ForcedMajorBlockWakeupTime =
            txTiming.forcedMajorBlockWakeupTime(fallbackTxStartTime)

        /** Block zero absorbs no deposits, so it never wakes up to decide on one. */
        override val mDepositDecisionWakeupTime: Option[DepositDecisionWakeupTime] = None
    }

    given (using CardanoNetwork.Section): Codec[BlockHeader.Minor] = deriveCodec[BlockHeader.Minor]

    final case class Minor(
        override val blockNum: BlockNumber,
        override val blockVersion: BlockVersion.Full,
        override val startTime: BlockCreationStartTime,
        override val endTime: BlockCreationEndTime,
        override val fallbackTxStartTime: FallbackTxStartTime,
        override val forcedMajorBlockWakeupTime: ForcedMajorBlockWakeupTime,
        override val mDepositDecisionWakeupTime: Option[DepositDecisionWakeupTime],
    ) extends BlockHeader,
          BlockType.Minor,
          NonFinal.Section {
        override transparent inline def header: BlockHeader.Minor = this
    }

    given (using CardanoNetwork.Section): Codec[BlockHeader.Major] = deriveCodec[BlockHeader.Major]

    final case class Major(
        override val blockNum: BlockNumber,
        override val blockVersion: BlockVersion.Full,
        override val startTime: BlockCreationStartTime,
        override val endTime: BlockCreationEndTime,
        override val fallbackTxStartTime: FallbackTxStartTime,
        override val forcedMajorBlockWakeupTime: ForcedMajorBlockWakeupTime,
        override val mDepositDecisionWakeupTime: Option[DepositDecisionWakeupTime],
    ) extends BlockHeader,
          BlockType.Major,
          NonFinal.Section {
        override transparent inline def header: BlockHeader.Major = this
    }

    given (using cardanoNetwork: CardanoNetwork.Section): Codec[Final] = deriveCodec[Final]
    final case class Final(
        override val blockNum: BlockNumber,
        override val blockVersion: BlockVersion.Full,
        override val startTime: BlockCreationStartTime,
        override val endTime: BlockCreationEndTime,
    ) extends BlockHeader,
          BlockType.Final {
        override transparent inline def header: BlockHeader.Final = this
    }

    type Next = BlockHeader & BlockType.Next
    type Intermediate = BlockHeader & BlockType.Intermediate
    type NonFinal = BlockHeader & BlockType.NonFinal & NonFinal.Section

    object Fields {
        trait HasBlockNum {
            def blockNum: BlockNumber
        }

        trait HasBlockVersion {
            def blockVersion: BlockVersion.Full
        }

        trait HasBlockStart {
            def startTime: BlockCreationStartTime
        }

        trait HasBlockEnd {
            def endTime: BlockCreationEndTime
        }

        trait NonFinal {
            def fallbackTxStartTime: FallbackTxStartTime
            def forcedMajorBlockWakeupTime: ForcedMajorBlockWakeupTime
            def mDepositDecisionWakeupTime: Option[DepositDecisionWakeupTime]
        }
    }

    import Fields.*

    trait Section extends BlockType, HasBlockNum, HasBlockVersion, HasBlockStart, HasBlockEnd {
        def header: BlockHeader

        final def nextHeaderFinal(
            newStartTime: BlockCreationStartTime,
            newEndTime: BlockCreationEndTime,
        ): BlockHeader.Final = BlockHeader.Final(
          blockNum = blockNum.increment,
          blockVersion = blockVersion.incrementMajor,
          startTime = newStartTime,
          endTime = newEndTime
        )
    }

    object NonFinal {
        trait Section extends BlockHeader.Section, Fields.NonFinal {
            final def nextHeaderIntermediate[F[_]: Monad](
                bhTracer: ContraTracer[F, BlockHeaderEvent],
                tmTracer: ContraTracer[F, TxTimingEvent]
            )(
                txTiming: TxTiming,
                newStartTime: BlockCreationStartTime,
                newEndTime: BlockCreationEndTime,
                mAbsorptionStartTime: Option[DepositAbsorptionStartTime],
            ): F[BlockHeader.Intermediate] =
                txTiming.blockCanStayMinor(tmTracer)(newEndTime, fallbackTxStartTime).flatMap {
                    canStayMinor =>
                        if canStayMinor then
                            nextHeaderMinor(bhTracer)(
                              newStartTime,
                              newEndTime,
                              mAbsorptionStartTime,
                            ).widen[BlockHeader.Intermediate]
                        else
                            nextHeaderMajor(bhTracer)(
                              txTiming,
                              newStartTime,
                              newEndTime,
                              mAbsorptionStartTime,
                            ).widen[BlockHeader.Intermediate]
                }

            final def nextHeaderMinor[F[_]: Monad](
                tracer: ContraTracer[F, BlockHeaderEvent]
            )(
                newStartTime: BlockCreationStartTime,
                newEndTime: BlockCreationEndTime,
                mAbsorptionStartTime: Option[DepositAbsorptionStartTime],
            ): F[BlockHeader.Minor] = {
                val newDepositDecisionWakeupTime =
                    mAbsorptionStartTime.map(t => DepositDecisionWakeupTime(t.convert))
                val header = BlockHeader.Minor(
                  blockNum = blockNum.increment,
                  blockVersion = blockVersion.incrementMinor,
                  startTime = newStartTime,
                  endTime = newEndTime,
                  fallbackTxStartTime = fallbackTxStartTime,
                  forcedMajorBlockWakeupTime = forcedMajorBlockWakeupTime,
                  mDepositDecisionWakeupTime = newDepositDecisionWakeupTime,
                )
                tracer
                    .traceWith(
                      BlockHeaderEvent
                          .NextMinor(forcedMajorBlockWakeupTime, newDepositDecisionWakeupTime)
                    )
                    .as(header)
            }

            final def nextHeaderMajor[F[_]: Monad](
                tracer: ContraTracer[F, BlockHeaderEvent]
            )(
                txTiming: TxTiming,
                newStartTime: BlockCreationStartTime,
                newEndTime: BlockCreationEndTime,
                mAbsorptionStartTime: Option[DepositAbsorptionStartTime],
            ): F[BlockHeader.Major] = {
                val newFallbackStartTime = txTiming.newFallbackStartTime(newEndTime)
                val newForcedMajorBlockWakeupTime =
                    txTiming.forcedMajorBlockWakeupTime(newFallbackStartTime)
                val newDepositDecisionWakeupTime =
                    mAbsorptionStartTime.map(t => DepositDecisionWakeupTime(t.convert))
                val header = BlockHeader.Major(
                  blockNum = blockNum.increment,
                  blockVersion = blockVersion.incrementMajor,
                  startTime = newStartTime,
                  endTime = newEndTime,
                  fallbackTxStartTime = newFallbackStartTime,
                  forcedMajorBlockWakeupTime = newForcedMajorBlockWakeupTime,
                  mDepositDecisionWakeupTime = newDepositDecisionWakeupTime,
                )
                tracer
                    .traceWith(
                      BlockHeaderEvent
                          .NextMajor(newForcedMajorBlockWakeupTime, newDepositDecisionWakeupTime)
                    )
                    .as(header)
            }
        }
    }
    object Initial {
        final transparent inline def blockNum: BlockNumber = BlockNumber.zero
        final transparent inline def blockVersion: BlockVersion.Full = BlockVersion.Full.zero

        /** Block zero's header travels as its end time alone; the reader rebuilds the rest from the
          * same [[TxTiming]] the writer used.
          */
        given blockHeaderInitialEncoder: Encoder[BlockHeader.Initial] with {
            override def apply(initBH: BlockHeader.Initial): Json =
                Json.obj("endTime" -> initBH.endTime.instant.toEpochMilli.asJson)
        }

        given blockHeaderInitialDecoder(using
            config: CardanoNetwork.Section,
            txTiming: TxTiming
        ): Decoder[BlockHeader.Initial] =
            Decoder.instance { c =>
                for {
                    millis <- c.downField("endTime").as[Long]
                    endTime = QuantizedInstant(config.slotConfig, Instant.ofEpochMilli(millis))
                } yield BlockHeader.Initial(BlockCreationEndTime(endTime))
            }
    }

}
