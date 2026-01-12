package hydrozoa.multisig.protocol.types

import cats.effect.IO
import cats.syntax.all.*
import com.suprnation.actor.ActorRef.ActorRef
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.multisig.ledger.virtual.commitment.KzgCommitment.KzgCommitment
import hydrozoa.multisig.protocol.types.LedgerEventId.ValidityFlag

enum Block {
    def id: Block.Number = this.header.blockNum
    def header: Block.Header

    case Initial(
        override val header: Block.Header.Initial
    ) extends Block

    // Only produced _between_ settlement transactions on L1 -- i.e., tracks updates on L2
    case Minor(
        override val header: Block.Header.Minor,
        override val body: Block.Body.Minor
    ) extends Block, Block.Body.Field

    // Only produced after settlement transactions on L1
    case Major(
        override val header: Block.Header.Major,
        override val body: Block.Body.Major
    ) extends Block, Block.Body.Field

    case Final(
        override val header: Block.Header.Final,
        override val body: Block.Body.Final
    ) extends Block, Block.Body.Field

    def nextBlock(
        newBody: Block.Body.Next,
        newTime: QuantizedInstant,
        newCommitment: KzgCommitment
    ): Block =
        header.nextBlock(newBody, newTime, newCommitment)
}

object Block {
    type Subscriber = ActorRef[IO, Block]

    type Next = Block.Minor | Block.Major | Block.Final

    extension (next: Next)
        def blockEvents: List[LedgerEventId] = next match {
            case Block.Minor(_, body) => body.events.map(_._1)
            case Block.Major(_, body) => body.events.map(_._1)
            case Block.Final(_, body) => body.events.map(_._1)
        }

    type Number = Number.Number
    enum Type:
        case Initial, Minor, Major, Final

    object Type {
        type Next = Type.Minor.type | Type.Major.type | Type.Final.type
    }

    enum Header(val blockType: Type) extends HeaderFields.Mandatory {
        case Initial(
            // TODO: this seems to be the same as `initializedOn`
            override val timeCreation: QuantizedInstant,
            override val commitment: KzgCommitment
        ) extends Header(Type.Initial), HeaderFields.InitialHeaderFields, HeaderFields.Commitment

        case Minor(
            override val blockNum: Number,
            override val blockVersion: Version.Full,
            override val timeCreation: QuantizedInstant,
            override val commitment: KzgCommitment
        ) extends Header(Type.Minor), HeaderFields.Commitment

        case Major(
            override val blockNum: Number,
            override val blockVersion: Version.Full,
            override val timeCreation: QuantizedInstant,
            override val commitment: KzgCommitment
        ) extends Header(Type.Major), HeaderFields.Commitment

        case Final(
            override val blockNum: Number,
            override val blockVersion: Version.Full,
            override val timeCreation: QuantizedInstant
        ) extends Header(Type.Final)

        def nextBlockNumber: Block.Number = this match {
            case Header.Initial(timeCreation, commitment) => Number.first
            case Header.Minor(blockNum, _, _, _)          => blockNum.increment
            case Header.Major(blockNum, _, _, _)          => blockNum.increment
            case Header.Final(blockNum, _, _)             => blockNum.increment
        }

        def nextHeader(
            newBlockType: Type.Next,
            newTime: QuantizedInstant,
            newCommitment: KzgCommitment
        ): Header = newBlockType match {
            case Type.Minor => nextHeaderMinor(newTime, newCommitment)
            case Type.Major => nextHeaderMajor(newTime, newCommitment)
            case Type.Final => nextHeaderFinal(newTime)
        }

        def nextBlock(
            body: Body.Next,
            newTime: QuantizedInstant,
            newCommitment: KzgCommitment
        ): Block =
            body match {
                case b: Body.Minor =>
                    Block.Minor(header = nextHeaderMinor(newTime, newCommitment), body = b)
                case b: Body.Major =>
                    Block.Major(header = nextHeaderMajor(newTime, newCommitment), body = b)
                case b: Body.Final =>
                    Block.Final(header = nextHeaderFinal(newTime), body = b)
            }

        private def nextHeaderMinor(
            newTime: QuantizedInstant,
            newCommitment: KzgCommitment
        ): Header.Minor =
            Header.Minor(
              blockNum = blockNum.increment,
              blockVersion = blockVersion.incrementMinor,
              timeCreation = newTime,
              commitment = newCommitment
            )

        private def nextHeaderMajor(
            newTime: QuantizedInstant,
            newCommitment: KzgCommitment
        ): Header.Major =
            Header.Major(
              blockNum = blockNum.increment,
              blockVersion = blockVersion.incrementMajor,
              timeCreation = newTime,
              commitment = newCommitment
            )

        private def nextHeaderFinal(
            newTime: QuantizedInstant
        ): Header.Final =
            Header.Final(
              blockNum = blockNum.increment,
              blockVersion = blockVersion.incrementMajor,
              timeCreation = newTime
            )
    }

    object HeaderFields {
        sealed trait Mandatory {
            def blockNum: Number
            def blockVersion: Version.Full
            def timeCreation: QuantizedInstant
        }

        sealed trait Commitment {
            def commitment: KzgCommitment
        }

        sealed trait InitialHeaderFields extends Mandatory {
            final override def blockNum: Number = Number.zero
            final override def blockVersion: Version.Full = Version.Full.zero
        }
    }

    // NOTE: (Peter, 2025-12-09) George has some comments on these in his Deposit/Refund branch.
    // In addition to those, we should also note that ledgerEventsRequired should contain an entry for _every_ peer,
    // regardless of if the peer submitted any _new_ events in this block; if there's no new events for a given peer,
    // the ledgerEventsRequired entry should match that of the previous block.
    //
    // But one block "property" is that every valid block should have `ledgerEventsRequired` entries that are
    // equal to the greatest entry in any of the List fields, if such an entry exists, and equal to the entry in the
    // previous block otherwise.
    enum Body {
        case Initial extends Body

        case Minor(
            override val events: List[(LedgerEventId, ValidityFlag)],
            override val depositsRefunded: List[LedgerEventId]
        ) extends Body, BodyFields.Minor

        case Major(
            override val events: List[(LedgerEventId, ValidityFlag)],
            override val depositsAbsorbed: List[LedgerEventId],
            override val depositsRefunded: List[LedgerEventId]
        ) extends Body, BodyFields.Major

        case Final(
            override val events: List[(LedgerEventId, ValidityFlag)],
            override val depositsRefunded: List[LedgerEventId]
        ) extends Body, BodyFields.Final
    }

    object Body {
        type Next = Body.Minor | Body.Major | Body.Final
        trait Field {
            def body: Block.Body
        }
    }

    private object BodyFields {
        sealed trait Minor extends Events, Deposits.Refunded

        sealed trait Major extends Events, Deposits.Absorbed, Deposits.Refunded

        sealed trait Final extends Events, Deposits.Refunded

        sealed trait Events {

            /** All the ledger events included in the block, i.e. L2 txs and deposit requests along
              * with the validity flag. The reason we have this one list is that the fact that
              * followers need to know the global order to replay the events in exactly the same
              * order to get the same result.
              *
              * TODO: invariant: the list should be unique
              */
            def events: List[(LedgerEventId, ValidityFlag)]
        }

        object Deposits {
            sealed trait Absorbed {
                def depositsAbsorbed: List[LedgerEventId]
            }

            sealed trait Refunded {
                def depositsRefunded: List[LedgerEventId]
            }
        }
    }

    object Number {
        opaque type Number = Int

        def apply(i: Int): Number = i

        val zero: Number = apply(0)

        /** Number of the first (non-initial) block, i.e. 1. */
        val first: Number = zero.increment

        given Conversion[Number, Int] = identity

        given Ordering[Number] with {
            override def compare(x: Number, y: Number): Int =
                x.compare(y)
        }

        extension (self: Number) def increment: Number = Number(self + 1)
        extension (self: Number) def decrement: Number = Number(self - 1)
    }

    object Version {
        type Full = Full.Full
        type Major = Major.Major
        type Minor = Minor.Minor

        object Full {
            opaque type Full = (Int, Int)

            def apply(i: Int, j: Int): Full = (i, j)

            val zero: Full = apply(0, 0)

            def unapply(self: Full): (Major, Minor) = (Major(self._1), Minor(self._2))

            given Conversion[Full, (Int, Int)] = identity

            given Ordering[Full] with {
                override def compare(x: Full, y: Full): Int =
                    x.compare(y)
            }

            extension (self: Full)
                def major: Major = Major(self._1)
                def minor: Minor = Minor(self._2)
                def incrementMajor: Full = Full(self._1 + 1, 0)
                def incrementMinor: Full = Full(self._1, self._2 + 1)
        }

        object Major {
            opaque type Major = Int

            def apply(i: Int): Major = i

            val zero: Major = apply(0)

            given Conversion[Major, Int] = identity

            given Ordering[Major] with {
                override def compare(x: Major, y: Major): Int =
                    x.compare(y)
            }

            extension (self: Major) def increment: Major = Major(self + 1)

            trait Produced {
                def majorVersionProduced: Major
            }
        }

        object Minor {
            opaque type Minor = Int

            def apply(i: Int): Minor = i

            val zero: Minor = apply(0)

            given Conversion[Minor, Int] = identity

            given Ordering[Minor] with {
                override def compare(x: Minor, y: Minor): Int =
                    x.compare(y)
            }

            extension (self: Minor) def increment: Minor = Minor(self + 1)
        }
    }
}
