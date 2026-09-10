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

sealed trait BlockHeader extends BlockHeader.Section {
    def asUnsigned: this.type & BlockStatus.Unsigned =
        this.asInstanceOf[this.type & BlockStatus.Unsigned]
    def asHardConfirmed: this.type & BlockStatus.HardConfirmed =
        this.asInstanceOf[this.type & BlockStatus.HardConfirmed]
}

object BlockHeader {

    final case class Initial(
        // Creation start time: when did the peers start negotiating the head config, moderated by peer 0.
        override val startTime: BlockCreationStartTime,
        // Creation end time: when did the moderator (peer 0) receive all the information
        // to create the head config and broadcast it to the peers.
        override val endTime: BlockCreationEndTime,
        override val fallbackTxStartTime: FallbackTxStartTime,
        override val forcedMajorBlockWakeupTime: ForcedMajorBlockWakeupTime,
        override val mDepositDecisionWakeupTime: Option[DepositDecisionWakeupTime],
    ) extends BlockHeader,
          BlockType.Initial,
          NonFinal.Section {
        override transparent inline def blockNum: BlockNumber = Initial.blockNum
        override transparent inline def blockVersion: BlockVersion.Full = Initial.blockVersion
        override transparent inline def header: BlockHeader.Initial = this
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

    /** Block-type-agnostic signature. An alias of the original Minor-scoped opaque so existing
      * rule-based code (which speaks `BlockHeader.Minor.HeaderSignature` for dispute-resolution
      * voting) keeps working unchanged. Fast-consensus soft-acks for Minor, Major, and Final blocks
      * all share it, as do the slow side's SEC signatures.
      *
      * TODO: untie from minor blocks. See
      * https://linear.app/gummiworm-labs/issue/GUM-334/untie-the-soft-ack-signature-type-from-minor-blocks
      */
    type HeaderSignature = Minor.HeaderSignature

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

        given blockHeaderInitialEncoder: Encoder[BlockHeader.Initial] with {
            def helper(f: BlockHeader.Initial => QuantizedInstant)(using
                bh: BlockHeader.Initial
            ): Json =
                f(bh).instant.toEpochMilli.asJson

            override def apply(initBH: BlockHeader.Initial): Json = {
                given BlockHeader.Initial = initBH

                Json.obj(
                  "startTime" -> helper(_.startTime),
                  "endTime" -> helper(_.endTime),
                  "fallbackTxStartTime" -> helper(_.fallbackTxStartTime),
                  "forcedMajorBlockWakeupTime" -> helper(_.forcedMajorBlockWakeupTime),
                  "depositDecisionWakeupTime" -> initBH.mDepositDecisionWakeupTime.fold(Json.Null)(
                    t => t.convert.instant.toEpochMilli.asJson
                  ),
                )
            }
        }

        given blockHeaderInitialDecoder(using
            config: CardanoNetwork.Section
        ): Decoder[BlockHeader.Initial] =
            Decoder.instance { c =>
                given HCursor = c

                def helper(fieldName: String)(using
                    c: HCursor
                ): Either[DecodingFailure, QuantizedInstant] =
                    for {
                        instant <- c
                            .downField(fieldName)
                            .as[Long]
                            .map(java.time.Instant.ofEpochMilli)
                        res = QuantizedInstant(config.slotConfig, instant)
                    } yield res

                for {
                    startTime <- helper("startTime")
                    endTime <- helper("endTime")
                    fbtx <- helper("fallbackTxStartTime")
                    fmbt <- c.downField("forcedMajorBlockWakeupTime").as[ForcedMajorBlockWakeupTime]
                    mDdwt <- c
                        .downField("depositDecisionWakeupTime")
                        .as[Option[DepositDecisionWakeupTime]]
                } yield BlockHeader.Initial(
                  BlockCreationStartTime(startTime),
                  BlockCreationEndTime(endTime),
                  FallbackTxStartTime(fbtx),
                  fmbt,
                  mDdwt,
                )
            }
    }

    object Minor {
        import scalus.uplc.builtin.ByteString

        type HeaderSignature = HeaderSignature.HeaderSignature

        object HeaderSignature:
            opaque type HeaderSignature = IArray[Byte]

            def apply(signature: IArray[Byte]): HeaderSignature = signature

            given Conversion[HeaderSignature, IArray[Byte]] = identity

            given Conversion[HeaderSignature, Array[Byte]] = sig =>
                IArray.genericWrapArray(sig).toArray

            given Conversion[HeaderSignature, ByteString] = sig => ByteString.fromArray(sig)

            extension (signature: HeaderSignature) def untagged: IArray[Byte] = identity(signature)

        object MultiSigned {
            trait Section extends BlockType.Minor {
                def headerMultiSigned: List[BlockHeader.Minor.HeaderSignature]
            }
        }
    }

}
