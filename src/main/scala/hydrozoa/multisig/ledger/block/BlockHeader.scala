package hydrozoa.multisig.ledger.block

import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.multisig.ledger.virtual.commitment.KzgCommitment
import hydrozoa.multisig.ledger.virtual.commitment.KzgCommitment.{KzgCommitment, asByteString}

sealed trait BlockHeader extends BlockHeader.Section

object BlockHeader {
    final case class Initial(
        override val startTime: QuantizedInstant,
        override val endTime: QuantizedInstant,
        override val kzgCommitment: KzgCommitment
    ) extends BlockHeader,
          BlockType.Initial {
        override transparent inline def blockNum: BlockNumber = Initial.blockNum
        override transparent inline def blockVersion: BlockVersion.Full = Initial.blockVersion
        override transparent inline def header: BlockHeader.Initial = this
    }

    final case class Minor(
        override val blockNum: BlockNumber,
        override val blockVersion: BlockVersion.Full,
        override val startTime: QuantizedInstant,
        override val endTime: QuantizedInstant,
        override val kzgCommitment: KzgCommitment
    ) extends BlockHeader,
          BlockType.Minor {
        override transparent inline def header: BlockHeader.Minor = this

        inline transparent def onchain: Minor.Onchain = Minor.Onchain(this)
        inline transparent def onchainMsg: Minor.Onchain.Serialized =
            Minor.Onchain.Serialized(onchain)
    }

    final case class Major(
        override val blockNum: BlockNumber,
        override val blockVersion: BlockVersion.Full,
        override val startTime: QuantizedInstant,
        override val endTime: QuantizedInstant,
        override val kzgCommitment: KzgCommitment
    ) extends BlockHeader,
          BlockType.Major {
        override transparent inline def header: BlockHeader.Major = this
    }

    final case class Final(
        override val blockNum: BlockNumber,
        override val blockVersion: BlockVersion.Full,
        override val startTime: QuantizedInstant,
        override val endTime: QuantizedInstant
    ) extends BlockHeader,
          BlockType.Final {
        override transparent inline def header: BlockHeader.Final = this
        override transparent inline def kzgCommitment: KzgCommitment = Final.kzgCommitment

    }

    type Next = BlockHeader & BlockType.Next

    type Intermediate = BlockHeader & BlockType.Intermediate

    trait Section extends BlockType {
        def header: BlockHeader
        def blockNum: BlockNumber
        def blockVersion: BlockVersion.Full
        def startTime: QuantizedInstant
        def endTime: QuantizedInstant
        def kzgCommitment: KzgCommitment

        final transparent inline def nextHeaderMinor(
            newStartTime: QuantizedInstant,
            newEndTime: QuantizedInstant,
            newKzgCommitment: KzgCommitment
        ): BlockHeader.Minor = BlockHeader.Minor(
          blockNum = blockNum.increment,
          blockVersion = blockVersion.incrementMinor,
          startTime = newStartTime,
          endTime = newEndTime,
          kzgCommitment = newKzgCommitment
        )

        final transparent inline def nextHeaderMajor(
            newStartTime: QuantizedInstant,
            newEndTime: QuantizedInstant,
            newKzgCommitment: KzgCommitment
        ): BlockHeader.Major = BlockHeader.Major(
          blockNum = blockNum.increment,
          blockVersion = blockVersion.incrementMajor,
          startTime = newStartTime,
          endTime = newEndTime,
          kzgCommitment = newKzgCommitment
        )

        final transparent inline def nextHeaderFinal(
            newStartTime: QuantizedInstant,
            newEndTime: QuantizedInstant,
        ): BlockHeader.Final = BlockHeader.Final(
          blockNum = blockNum.increment,
          blockVersion = blockVersion.incrementMajor,
          startTime = newStartTime,
          endTime = newEndTime,
        )
    }
    object Initial {
        final transparent inline def blockNum: BlockNumber = BlockNumber.zero
        final transparent inline def blockVersion: BlockVersion.Full = BlockVersion.Full.zero
    }

    object Minor {
        import hydrozoa.rulebased.ledger.dapp.state.VoteState
        import scalus.builtin.{FromData, ToData}
        import scalus.ledger.api.v3.PosixTime
        import scalus.builtin.ByteString

        final case class Onchain(
            blockNum: BigInt,
            startTime: PosixTime,
            versionMajor: BigInt,
            versionMinor: BigInt,
            commitment: VoteState.KzgCommitment
        ) derives FromData,
              ToData

        object Onchain {
            import scalus.builtin.Data.toData
            import scalus.builtin.Builtins.serialiseData

            def apply(offchainHeader: BlockHeader.Intermediate): Onchain =
                import offchainHeader.*
                new Onchain(
                  blockNum = BigInt(blockNum.convert),
                  startTime = startTime.instant.toEpochMilli,
                  versionMajor = BigInt(blockVersion.major.convert),
                  versionMinor = BigInt(blockVersion.minor.convert),
                  commitment = kzgCommitment.asByteString
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
