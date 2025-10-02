package hydrozoa.multisig.protocol.types

import hydrozoa.multisig.ledger.virtual.commitment.KzgCommitment.KzgCommitment

import com.suprnation.actor.ActorRef.ActorRef

import cats.effect.IO
import cats.syntax.all._

import scala.concurrent.duration.FiniteDuration

enum Block {
    def id: Block.Number = this.header.blockNum
    def header: Block.Header

    case Initial(
        override val header: Block.Header.Initial
    ) extends Block

    case Minor(
        override val header: Block.Header.Minor,
        override val body: Block.Body.Minor
    ) extends Block, Block.Body.Field

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
        newTime: FiniteDuration,
        newCommitment: KzgCommitment
    ): Block =
        header.nextBlock(newBody, newTime, newCommitment)
}

object Block {
    type Subscriber = ActorRef[IO, Block]

    type Next = Block.Minor | Block.Major | Block.Final

    type Number = Number.Number
    enum Type:
        case Initial, Minor, Major, Final

    object Type {
        type Next = Type.Minor.type | Type.Major.type | Type.Final.type
    }

    enum Header(val blockType: Type) extends HeaderFields.Mandatory {
        case Initial(
            override val timeCreation: FiniteDuration,
            override val commitment: KzgCommitment
        ) extends Header(Type.Initial), HeaderFields.InitialHeaderFields, HeaderFields.Commitment

        case Minor(
            override val blockNum: Number,
            override val blockVersion: Version.Full,
            override val timeCreation: FiniteDuration,
            override val commitment: KzgCommitment
        ) extends Header(Type.Minor), HeaderFields.Commitment

        case Major(
            override val blockNum: Number,
            override val blockVersion: Version.Full,
            override val timeCreation: FiniteDuration,
            override val commitment: KzgCommitment
        ) extends Header(Type.Major), HeaderFields.Commitment

        case Final(
            override val blockNum: Number,
            override val blockVersion: Version.Full,
            override val timeCreation: FiniteDuration
        ) extends Header(Type.Final)

        def nextHeader(
            newBlockType: Type.Next,
            newTime: FiniteDuration,
            newCommitment: KzgCommitment
        ): Header = newBlockType match {
            case Type.Minor => nextHeaderMinor(newTime, newCommitment)
            case Type.Major => nextHeaderMajor(newTime, newCommitment)
            case Type.Final => nextHeaderFinal(newTime)
        }

        def nextBlock(
            body: Body.Next,
            newTime: FiniteDuration,
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
            newTime: FiniteDuration,
            newCommitment: KzgCommitment
        ): Header.Minor =
            Header.Minor(
              blockNum = blockNum.increment,
              blockVersion = blockVersion.incrementMinor,
              timeCreation = newTime,
              commitment = newCommitment
            )

        private def nextHeaderMajor(
            newTime: FiniteDuration,
            newCommitment: KzgCommitment
        ): Header.Major =
            Header.Major(
              blockNum = blockNum.increment,
              blockVersion = blockVersion.incrementMajor,
              timeCreation = newTime,
              commitment = newCommitment
            )

        private def nextHeaderFinal(
            newTime: FiniteDuration
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
            def timeCreation: FiniteDuration
        }

        sealed trait Commitment {
            def commitment: KzgCommitment
        }

        sealed trait InitialHeaderFields extends Mandatory {
            override def blockNum: Number = Number(0)
            override def blockVersion: Version.Full = Version.Full(0, 0)
        }
    }

    enum Body {
        case Initial extends Body

        case Minor(
            override val ledgerEventsRequired: Map[Peer.Number, LedgerEvent.Number],
            override val transactionsValid: List[LedgerEvent.Id],
            override val transactionsInvalid: List[LedgerEvent.Id],
            override val depositsRegistered: List[LedgerEvent.Id],
            override val depositsRejected: List[LedgerEvent.Id],
            override val depositsRefunded: List[LedgerEvent.Id]
        ) extends Body, BodyFields.Minor

        case Major(
            override val ledgerEventsRequired: Map[Peer.Number, LedgerEvent.Number],
            override val transactionsValid: List[LedgerEvent.Id],
            override val transactionsInvalid: List[LedgerEvent.Id],
            override val depositsRegistered: List[LedgerEvent.Id],
            override val depositsRejected: List[LedgerEvent.Id],
            override val depositsAbsorbed: List[LedgerEvent.Id],
            override val depositsRefunded: List[LedgerEvent.Id]
        ) extends Body, BodyFields.Major

        case Final(
            override val ledgerEventsRequired: Map[Peer.Number, LedgerEvent.Number],
            override val transactionsValid: List[LedgerEvent.Id],
            override val transactionsInvalid: List[LedgerEvent.Id],
            override val depositsRejected: List[LedgerEvent.Id],
            override val depositsRefunded: List[LedgerEvent.Id]
        ) extends Body, BodyFields.Final
    }

    object Body {
        type Next = Body.Minor | Body.Major | Body.Final
        trait Field {
            def body: Block.Body
        }
    }

    object BodyFields {
        sealed trait Minor
            extends LedgerEventsRequired,
              Transactions,
              Deposits.Registered,
              Deposits.Rejected,
              Deposits.Refunded

        sealed trait Major
            extends LedgerEventsRequired,
              Transactions,
              Deposits.Registered,
              Deposits.Rejected,
              Deposits.Absorbed,
              Deposits.Refunded

        sealed trait Final
            extends LedgerEventsRequired,
              Transactions,
              Deposits.Rejected,
              Deposits.Refunded

        sealed trait LedgerEventsRequired {
            def ledgerEventsRequired: Map[Peer.Number, LedgerEvent.Number]
        }

        sealed trait Transactions {
            def transactionsValid: List[LedgerEvent.Id]
            def transactionsInvalid: List[LedgerEvent.Id]
        }

        object Deposits {
            sealed trait Registered {
                def depositsRegistered: List[LedgerEvent.Id]
            }

            sealed trait Rejected {
                def depositsRejected: List[LedgerEvent.Id]
            }

            sealed trait Absorbed {
                def depositsAbsorbed: List[LedgerEvent.Id]
            }

            sealed trait Refunded {
                def depositsRefunded: List[LedgerEvent.Id]
            }
        }
    }

    object Number {
        opaque type Number = Int

        def apply(i: Int): Number = i

        given Conversion[Number, Int] = identity

        given Ordering[Number] with {
            override def compare(x: Number, y: Number): Int =
                x.compare(y)
        }

        extension (self: Number) def increment: Number = Number(self + 1)
    }

    object Version {
        type Full = Full.Full
        type Major = Major.Major
        type Minor = Minor.Minor

        object Full {
            opaque type Full = (Int, Int)

            def apply(i: Int, j: Int): Full = (i, j)

            def unapply(self: Full): (Major, Minor) = (Major(self._1), Minor(self._2))

            given Conversion[Full, (Int, Int)] = identity

            given Ordering[Full] with {
                override def compare(x: Full, y: Full): Int =
                    x.compare(y)
            }

            extension (self: Full)
                def major: Major = Major(self._1)
                def minor: Minor = Minor(self._2)
                def incrementMajor: Full = Full(self._1 + 1, self._2)
                def incrementMinor: Full = Full(self._1, self._2 + 1)
        }

        object Major {
            opaque type Major = Int

            def apply(i: Int): Major = i

            given Conversion[Major, Int] = identity

            given Ordering[Major] with {
                override def compare(x: Major, y: Major): Int =
                    x.compare(y)
            }

            extension (self: Major) def increment: Major = Major(self + 1)
        }

        object Minor {
            opaque type Minor = Int

            def apply(i: Int): Minor = i

            given Conversion[Minor, Int] = identity

            given Ordering[Minor] with {
                override def compare(x: Minor, y: Minor): Int =
                    x.compare(y)
            }

            extension (self: Minor) def increment: Minor = Minor(self + 1)
        }
    }
}
