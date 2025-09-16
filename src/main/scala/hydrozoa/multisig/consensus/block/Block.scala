package hydrozoa.multisig.consensus.block

import hydrozoa.multisig.ledger.KzgCommitment

import scala.concurrent.duration.FiniteDuration
import scala.math.Ordered.orderingToOrdered

enum Block {
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
        newVirtualUtxos: KzgCommitment
    ): Block =
        header.nextBlock(newBody, newTime, newVirtualUtxos)
}

object Block {
    type Next = Block.Minor | Block.Major | Block.Final

    type Number = Number.Number
    enum Type:
        case Initial, Minor, Major, Final

    object Type {
        type Next = Type.Minor.type | Type.Major.type | Type.Final.type
    }

    enum Header(val blockType: Type) extends HeaderFields.Mandatory {
        case Initial (
            override val timeCreation: FiniteDuration,
            override val virtualUtxos: KzgCommitment
        ) extends Header(Type.Initial), HeaderFields.InitialHeaderFields, HeaderFields.VirtualUtxos

        case Minor(
            override val blockNum: Number,
            override val blockVersion: Version.Full,
            override val timeCreation: FiniteDuration,
            override val virtualUtxos: KzgCommitment
        ) extends Header(Type.Minor), HeaderFields.VirtualUtxos

        case Major(
            override val blockNum: Number,
            override val blockVersion: Version.Full,
            override val timeCreation: FiniteDuration,
            override val virtualUtxos: KzgCommitment
        ) extends Header(Type.Major), HeaderFields.VirtualUtxos

        case Final(
            override val blockNum: Number,
            override val blockVersion: Version.Full,
            override val timeCreation: FiniteDuration
        ) extends Header(Type.Final)

        def nextHeader(
            newBlockType: Type.Next,
            newTime: FiniteDuration,
            newVirtualUtxos: KzgCommitment
        ): Header = newBlockType match {
            case Type.Minor => nextHeaderMinor(newTime, newVirtualUtxos)
            case Type.Major => nextHeaderMajor(newTime, newVirtualUtxos)
            case Type.Final => nextHeaderFinal(newTime)
        }

        def nextBlock(body: Body.Next, newTime: FiniteDuration, newVirtualUtxos: KzgCommitment): Block =
            body match {
                case b: Body.Minor =>
                    Block.Minor(header = nextHeaderMinor(newTime, newVirtualUtxos), body = b)
                case b: Body.Major =>
                    Block.Major(header = nextHeaderMajor(newTime, newVirtualUtxos), body = b)
                case b: Body.Final =>
                    Block.Final(header = nextHeaderFinal(newTime), body = b)
            }

        private def nextHeaderMinor(
            newTime: FiniteDuration,
            newVirtualUtxos: KzgCommitment
        ): Header.Minor =
            Header.Minor(
              blockNum = blockNum.increment,
              blockVersion = blockVersion.incrementMinor,
              timeCreation = newTime,
              virtualUtxos = newVirtualUtxos
            )

        private def nextHeaderMajor(
            newTime: FiniteDuration,
            newVirtualUtxos: KzgCommitment
        ): Header.Major =
            Header.Major(
              blockNum = blockNum.increment,
              blockVersion = blockVersion.incrementMajor,
              timeCreation = newTime,
              virtualUtxos = newVirtualUtxos
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

        sealed trait VirtualUtxos {
            def virtualUtxos: KzgCommitment
        }

        sealed trait InitialHeaderFields extends Mandatory {
            override def blockNum: Number = Number(0)
            override def blockVersion: Version.Full = Version.Full(0, 0)
        }
    }

    enum Body {
        case Initial extends Body

        case Minor(
            override val transactionsValid: List[Unit],
            override val transactionsInvalid: List[Unit],
            override val depositsRegistered: List[Unit],
            override val depositsRejected: List[Unit],
            override val depositsRefunded: List[Unit]
        ) extends Body, BodyFields.Minor

        case Major(
            override val transactionsValid: List[Unit],
            override val transactionsInvalid: List[Unit],
            override val depositsRegistered: List[Unit],
            override val depositsRejected: List[Unit],
            override val depositsAbsorbed: List[Unit],
            override val depositsRefunded: List[Unit]
        ) extends Body, BodyFields.Major

        case Final(
            override val transactionsValid: List[Unit],
            override val transactionsInvalid: List[Unit],
            override val depositsRejected: List[Unit],
            override val depositsRefunded: List[Unit]
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
            extends Transactions,
              Deposits.Registered,
              Deposits.Rejected,
              Deposits.Refunded

        sealed trait Major
            extends Transactions,
              Deposits.Registered,
              Deposits.Rejected,
              Deposits.Absorbed,
              Deposits.Refunded

        sealed trait Final extends Transactions, Deposits.Rejected, Deposits.Refunded

        sealed trait Transactions {
            def transactionsValid: List[Unit]
            def transactionsInvalid: List[Unit]
        }

        object Deposits {
            sealed trait Registered {
                def depositsRegistered: List[Unit]
            }

            sealed trait Rejected {
                def depositsRejected: List[Unit]
            }

            sealed trait Absorbed {
                def depositsAbsorbed: List[Unit]
            }

            sealed trait Refunded {
                def depositsRefunded: List[Unit]
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
