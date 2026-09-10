package hydrozoa.multisig.ledger.block

import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.{BlockCreationEndTime, BlockCreationStartTime}
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.multisig.ledger.event.RequestHash
import io.circe.*
import io.circe.generic.semiauto.*

sealed trait BlockBrief extends BlockBrief.Section {

    def asUnsigned: this.type & BlockStatus.Unsigned =
        this.asInstanceOf[this.type & BlockStatus.Unsigned]
    def asHardConfirmed: this.type & BlockStatus.HardConfirmed =
        this.asInstanceOf[this.type & BlockStatus.HardConfirmed]
    def asSoftConfirmed: this.type & BlockStatus.SoftConfirmed =
        this.asInstanceOf[this.type & BlockStatus.SoftConfirmed]
}

object BlockBrief {
    // N.B.: technically we only need the cardano network for the decoder.
    given (using cardanoNetwork: CardanoNetwork.Section): Codec[BlockBrief] =
        deriveCodec[BlockBrief]
    given (using cardanoNetwork: CardanoNetwork.Section): Codec[BlockBrief.Initial] =
        deriveCodec[BlockBrief.Initial]
    given bbMinorCodec(using CardanoNetwork.Section): Codec[BlockBrief.Minor] =
        deriveCodec[BlockBrief.Minor]
    given bbMajorCodec(using CardanoNetwork.Section): Codec[BlockBrief.Major] =
        deriveCodec[BlockBrief.Major]
    given (using CardanoNetwork.Section): Codec[BlockBrief.Final] = deriveCodec[BlockBrief.Final]

    final case class Initial(
        override val header: BlockHeader.Initial,
        override val blockHash: BlockHash
    ) extends BlockBrief,
          BlockType.Initial {
        override transparent inline def blockBrief: BlockBrief.Initial = this
        override transparent inline def body: BlockBody.Initial.type = BlockBody.Initial
    }

    object Initial {

        /** Build the brief, deriving its digest — what a producer does. The generated two-argument
          * `apply` keeps a digest that arrived with the brief instead, so a reader can compare the
          * claim against one it derived itself.
          */
        def apply(header: BlockHeader.Initial): Initial =
            new Initial(header, BlockHash(header, BlockBody.Initial))
    }

    final case class Minor(
        override val header: BlockHeader.Minor,
        override val body: BlockBody.Minor,
        override val blockHash: BlockHash
    ) extends BlockBrief,
          BlockType.Minor {
        override transparent inline def blockBrief: BlockBrief.Minor = this
    }

    object Minor {

        /** Build the brief, deriving its digest — see [[Initial.apply]]. */
        def apply(header: BlockHeader.Minor, body: BlockBody.Minor): Minor =
            new Minor(header, body, BlockHash(header, body))
    }

    final case class Major(
        override val header: BlockHeader.Major,
        override val body: BlockBody.Major,
        override val blockHash: BlockHash
    ) extends BlockBrief,
          BlockType.Major {
        override transparent inline def blockBrief: BlockBrief.Major = this
    }

    object Major {

        /** Build the brief, deriving its digest — see [[Initial.apply]]. */
        def apply(header: BlockHeader.Major, body: BlockBody.Major): Major =
            new Major(header, body, BlockHash(header, body))
    }

    final case class Final(
        override val header: BlockHeader.Final,
        override val body: BlockBody.Final,
        override val blockHash: BlockHash
    ) extends BlockBrief,
          BlockType.Final {
        override transparent inline def blockBrief: BlockBrief.Final = this
    }

    object Final {

        /** Build the brief, deriving its digest — see [[Initial.apply]]. */
        def apply(header: BlockHeader.Final, body: BlockBody.Final): Final =
            new Final(header, body, BlockHash(header, body))
    }

    type Next = BlockBrief & BlockType.Next
    type Intermediate = BlockBrief & BlockType.Intermediate
    type NonFinal = BlockBrief & BlockType.NonFinal

    trait Section extends BlockType, BlockHeader.Section, BlockBody.Section {
        import hydrozoa.multisig.ledger.event.RequestId
        import RequestId.ValidityFlag

        def blockBrief: BlockBrief

        /** This block's content digest — see [[BlockHash]].
          *
          * A brief is where header and body meet, which is why the digest lives here and not on
          * [[BlockHeader]]: `nextHeaderMinor` and its siblings derive block N+1's header from N's
          * header plus timing, *before* N+1's body exists, so a header field covering the body
          * could never be filled.
          *
          * **Stored, and never trusted.** The brief carries the value on the wire and into the
          * `Block` journal, but a stored digest is a claim: a peer that rebuilds the block derives
          * its own and compares (`JointLedger.panicOnMismatchWithExpectedBrief`). What makes the
          * claim worth anything is [[signingBytes]] — every head peer's soft-ack signs it.
          */
        def blockHash: BlockHash

        override transparent inline def blockNum: BlockNumber = header.blockNum
        override transparent inline def blockVersion: BlockVersion.Full = header.blockVersion
        override transparent inline def startTime: BlockCreationStartTime = header.startTime
        override transparent inline def endTime: BlockCreationEndTime = header.endTime

        override transparent inline def requests: List[(RequestId, RequestHash, ValidityFlag)] =
            body.requests
        override transparent inline def depositsAbsorbed: List[RequestId] =
            body.depositsAbsorbed
        override transparent inline def depositsRejected: List[RequestId] =
            body.depositsRejected

        /** Canonical byte representation a head peer's soft acknowledgment signs over (Ed25519).
          *
          * It authenticates the block's whole content through [[blockHash]], with the two version
          * components beside it so a ratchet can *order* two signed statements without recomputing
          * a digest. The slow cycle's dispute-script-facing bytes live separately on
          * [[hydrozoa.multisig.ledger.stack.StandaloneEvacuationCommitment.Onchain.Serialized]].
          */
        final def signingBytes: BlockBrief.SignedDigest.Serialized =
            BlockBrief.SignedDigest.Serialized(BlockBrief.SignedDigest.Onchain(this))
    }

    object Section {
        type Next = Section & BlockType.Next
        type Intermediate = Section & BlockType.Intermediate
        type NonFinal = Section & BlockType.NonFinal
    }

    /** The canonical bytes a head peer's soft acknowledgment signs over: the block's content
      * digest, with the two version components beside it.
      *
      * `blockNum` and `startTime` are inside the [[BlockHash]] preimage, so a signature made over
      * block N still cannot be replayed as block M without them;
      * [[hydrozoa.multisig.consensus.ack.SoftAck]] also carries the block number as a plain field,
      * so anything wanting it has it without parsing signed bytes. The versions are duplicated in
      * the preimage on purpose — a digest gives an ordering on nothing, and a ratchet must read
      * `versionMajor` for equality and `versionMinor` for strict increase.
      *
      * No on-chain consumer reads this — Hydrozoa's L1 scripts speak the SEC's `Onchain` datum, not
      * the soft-ack bytes. We still derive `Serialized` via scalus' `serialiseData` for canonical
      * byte determinism and toolchain consistency with the SEC.
      */
    object SignedDigest {
        import scalus.uplc.builtin.{ByteString, FromData, ToData}
        import scalus.uplc.builtin.Builtins.serialiseData
        import scalus.uplc.builtin.Data.toData

        final case class Onchain(
            versionMajor: BigInt,
            versionMinor: BigInt,
            blockHash: ByteString,
        ) derives FromData,
              ToData

        object Onchain {
            def apply(brief: BlockBrief.Section): Onchain =
                new Onchain(
                  versionMajor = BigInt(brief.blockVersion.major.convert),
                  versionMinor = BigInt(brief.blockVersion.minor.convert),
                  blockHash = ByteString.fromArray(brief.blockHash.bytes)
                )
        }

        type Serialized = Serialized.Serialized

        object Serialized {
            opaque type Serialized = IArray[Byte]

            def apply(onchain: Onchain): Serialized =
                IArray.from(serialiseData(onchain.toData).bytes)

            given Conversion[Serialized, IArray[Byte]] = identity

            given Conversion[Serialized, Array[Byte]] = msg => IArray.genericWrapArray(msg).toArray

            given Conversion[Serialized, ByteString] = msg => ByteString.fromArray(msg)

            extension (msg: Serialized) def untagged: IArray[Byte] = identity(msg)
        }
    }
}
