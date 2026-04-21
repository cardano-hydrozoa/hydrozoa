package hydrozoa.multisig.ledger.block

import hydrozoa.config.head.multisig.timing.TxTiming
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.given
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.{BlockCreationEndTime, BlockCreationStartTime, FallbackTxStartTime, MajorBlockWakeupTime}
import hydrozoa.config.head.multisig.timing.TxTiming.RequestTimes.DepositAbsorptionStartTime
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.lib.cardano.cip116.JsonCodecs.CIP0116.Conway.given
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.multisig.ledger.commitment.KzgCommitment
import io.circe.*
import io.circe.generic.semiauto.*
import io.circe.syntax.*

import KzgCommitment.KzgCommitment

sealed trait BlockHeader extends BlockHeader.Section {
    def asUnsigned: this.type & BlockStatus.Unsigned =
        this.asInstanceOf[this.type & BlockStatus.Unsigned]
    def asMultiSigned: this.type & BlockStatus.MultiSigned =
        this.asInstanceOf[this.type & BlockStatus.MultiSigned]
}

object BlockHeader {
    final case class Initial(
        // Creation start time: when did the peers start negotiating the head config, moderated by peer 0.
        override val startTime: BlockCreationStartTime,
        // Creation end time: when did the moderator (peer 0) receive all the information
        // to create the head config and broadcast it to the peers.
        override val endTime: BlockCreationEndTime,
        override val fallbackTxStartTime: FallbackTxStartTime,
        override val majorBlockWakeupTime: MajorBlockWakeupTime,
        override val kzgCommitment: KzgCommitment
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
        override val majorBlockWakeupTime: MajorBlockWakeupTime,
        override val kzgCommitment: KzgCommitment
    ) extends BlockHeader,
          BlockType.Minor,
          NonFinal.Section {
        override transparent inline def header: BlockHeader.Minor = this

        inline transparent def onchain: Minor.Onchain = Minor.Onchain(this)
        inline transparent def onchainMsg: Minor.Onchain.Serialized =
            Minor.Onchain.Serialized(onchain)
    }

    given (using CardanoNetwork.Section): Codec[BlockHeader.Major] = deriveCodec[BlockHeader.Major]

    final case class Major(
        override val blockNum: BlockNumber,
        override val blockVersion: BlockVersion.Full,
        override val startTime: BlockCreationStartTime,
        override val endTime: BlockCreationEndTime,
        override val fallbackTxStartTime: FallbackTxStartTime,
        override val majorBlockWakeupTime: MajorBlockWakeupTime,
        override val kzgCommitment: KzgCommitment
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
        override transparent inline def kzgCommitment: KzgCommitment = Final.kzgCommitment

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

        trait HasKzgCommitment {
            def kzgCommitment: KzgCommitment
        }

        trait NonFinal {
            def fallbackTxStartTime: FallbackTxStartTime
            def majorBlockWakeupTime: MajorBlockWakeupTime
        }
    }

    import Fields.*

    trait Section
        extends BlockType,
          HasBlockNum,
          HasBlockVersion,
          HasBlockStart,
          HasBlockEnd,
          HasKzgCommitment {
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
            final def nextHeaderIntermediate(
                txTiming: TxTiming,
                newStartTime: BlockCreationStartTime,
                newEndTime: BlockCreationEndTime,
                mAbsorptionStartTime: Option[DepositAbsorptionStartTime],
                newKzgCommitment: KzgCommitment
            ): BlockHeader.Intermediate =
                if txTiming.blockCanStayMinor(newEndTime, fallbackTxStartTime)
                then
                    nextHeaderMinor(
                      newStartTime,
                      newEndTime,
                      mAbsorptionStartTime,
                      newKzgCommitment
                    )
                else
                    nextHeaderMajor(
                      txTiming,
                      newStartTime,
                      newEndTime,
                      mAbsorptionStartTime,
                      newKzgCommitment
                    )

            final def nextHeaderMinor(
                newStartTime: BlockCreationStartTime,
                newEndTime: BlockCreationEndTime,
                mAbsorptionStartTime: Option[DepositAbsorptionStartTime],
                newKzgCommitment: KzgCommitment
            ): BlockHeader.Minor = {
                val newMajorBlockWakeupTime =
                    TxTiming.majorBlockWakeupTime(majorBlockWakeupTime, mAbsorptionStartTime)
                BlockHeader.Minor(
                  blockNum = blockNum.increment,
                  blockVersion = blockVersion.incrementMinor,
                  startTime = newStartTime,
                  endTime = newEndTime,
                  fallbackTxStartTime = fallbackTxStartTime,
                  majorBlockWakeupTime = newMajorBlockWakeupTime,
                  kzgCommitment = newKzgCommitment
                )
            }

            final def nextHeaderMajor(
                txTiming: TxTiming,
                newStartTime: BlockCreationStartTime,
                newEndTime: BlockCreationEndTime,
                mAbsorptionStartTime: Option[DepositAbsorptionStartTime],
                newKzgCommitment: KzgCommitment
            ): BlockHeader.Major = {
                val newFallbackStartTime = txTiming.newFallbackStartTime(newEndTime)
                val newForcedMajorBlockTime = txTiming.forcedMajorBlockTime(newFallbackStartTime)
                val newMajorBlockWakeupTime =
                    TxTiming.majorBlockWakeupTime(newForcedMajorBlockTime, mAbsorptionStartTime)
                BlockHeader.Major(
                  blockNum = blockNum.increment,
                  blockVersion = blockVersion.incrementMajor,
                  startTime = newStartTime,
                  endTime = newEndTime,
                  fallbackTxStartTime = newFallbackStartTime,
                  majorBlockWakeupTime = newMajorBlockWakeupTime,
                  kzgCommitment = newKzgCommitment
                )
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
                  "majorBlockWakeupTime" -> helper(_.majorBlockWakeupTime),
                  "kzgCommitment" -> initBH.kzgCommitment.asJson
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
                    mbwt <- helper("majorBlockWakeupTime")
                    kzg <- c.downField("kzgCommitment").as[KzgCommitment]
                } yield BlockHeader.Initial(
                  BlockCreationStartTime(startTime),
                  BlockCreationEndTime(endTime),
                  FallbackTxStartTime(fbtx),
                  MajorBlockWakeupTime(mbwt),
                  kzg
                )
            }
    }

    object Minor {
        import hydrozoa.rulebased.ledger.l1.state.VoteState
        import scalus.uplc.builtin.{FromData, ToData}
        import scalus.cardano.onchain.plutus.v3.PosixTime
        import scalus.uplc.builtin.ByteString

        final case class Onchain(
            blockNum: BigInt,
            startTime: PosixTime,
            versionMajor: BigInt,
            versionMinor: BigInt,
            commitment: VoteState.KzgCommitment
        ) derives FromData,
              ToData

        object Onchain {
            import scalus.uplc.builtin.Data.toData
            import scalus.uplc.builtin.Builtins.serialiseData

            def apply(offchainHeader: BlockHeader.Intermediate): Onchain =
                import offchainHeader.*
                new Onchain(
                  blockNum = BigInt(blockNum.convert),
                  startTime = startTime.instant.toEpochMilli,
                  versionMajor = BigInt(blockVersion.major.convert),
                  versionMinor = BigInt(blockVersion.minor.convert),
                  commitment = kzgCommitment
                )

            type Serialized = Serialized.Serialized

            object Serialized {
                opaque type Serialized = IArray[Byte]

                def apply(onchainHeader: Onchain): Serialized =
                    IArray.from(serialiseData(onchainHeader.toData).bytes)

                given Conversion[Serialized, IArray[Byte]] = identity

                given Conversion[Serialized, Array[Byte]] = msg =>
                    IArray.genericWrapArray(msg).toArray

                given Conversion[Serialized, ByteString] = msg => ByteString.fromArray(msg)

                extension (msg: Serialized) def untagged: IArray[Byte] = identity(msg)

                trait Section {
                    def headerSerialized: BlockHeader.Minor.Onchain.Serialized
                }
            }
        }

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

    object Final {
        lazy val kzgCommitment: KzgCommitment = KzgCommitment.empty
    }
}
