package hydrozoa.multisig.protocol

import hydrozoa.lib.actor.SyncRequest
import hydrozoa.multisig.ledger
import hydrozoa.multisig.ledger.dapp.tx.{DepositTx, RefundTx}
import hydrozoa.multisig.ledger.virtual.GenesisObligation
import hydrozoa.multisig.ledger.virtual.commitment.KzgCommitment.KzgCommitment
import hydrozoa.multisig.protocol.types.Block

import com.suprnation.actor.ActorRef.ActorRef

import cats.effect.{Deferred, IO}
import cats.syntax.all._

import scala.concurrent.duration.FiniteDuration

object LedgerProtocol {
    type LedgerEvent = VirtualTransaction | RegisterDeposit

    object DappLedger {
        type DappLedgerRef = Ref
        type Ref = ActorRef[IO, Request]
        type Request = RegisterDeposit
    }

    object VirtualLedger {
        type VirtualLedgerRef = Ref
        type Ref = ActorRef[IO, Request]
        type Request = VirtualTransaction
    }

    object JointLedger {
        type JointLedgerRef = Ref
        type Ref = ActorRef[IO, Request]
        type Request = RegisterDeposit | VirtualTransaction | CompleteBlock
    }

    final case class RegisterDeposit(
        txSerialized: ledger.DappLedger.Tx.Serialized,
        override val dResponse: Deferred[IO, Either[RegisterDeposit.Error, RegisterDeposit.Success]]
    ) extends SyncRequest[IO, RegisterDeposit.Error, RegisterDeposit.Success]

    object RegisterDeposit {
        def apply(txSerialized: ledger.DappLedger.Tx.Serialized): IO[RegisterDeposit] = for {
            deferredResponse <- Deferred[IO, Either[Error, Success]]
        } yield RegisterDeposit(txSerialized, deferredResponse)

        final case class Success(
            genesisObligations: List[GenesisObligation],
            refundTxs: List[RefundTx.PostDated]
        )

        type Error = DepositTx.ParseError
    }

    final case class VirtualTransaction(
        txSerialized: ledger.VirtualLedger.Tx.Serialized,
        override val dResponse: Deferred[
          IO,
          Either[VirtualTransaction.Error, VirtualTransaction.Success]
        ]
    ) extends SyncRequest[IO, VirtualTransaction.Error, VirtualTransaction.Success]

    object VirtualTransaction {
        def apply(txSerialized: ledger.VirtualLedger.Tx.Serialized): IO[VirtualTransaction] = for {
            deferredResponse <- Deferred[IO, Either[Error, Success]]
        } yield VirtualTransaction(txSerialized, deferredResponse)

        final case class Success(
            payoutObligations: List[ledger.JointLedger.PayoutObligation]
        )

        type Error = ledger.VirtualLedger.CborParseError |
            ledger.VirtualLedger.TransactionInvalidError
    }

    final case class CompleteBlock(
        timeCreation: FiniteDuration,
        override val dResponse: Deferred[IO, Either[CompleteBlock.Error, CompleteBlock.Success]]
    ) extends SyncRequest[IO, CompleteBlock.Error, CompleteBlock.Success]

    object CompleteBlock {
        def apply(timeCreation: FiniteDuration): IO[CompleteBlock] = for {
            deferredResponse <- Deferred[IO, Either[Error, Success]]
        } yield CompleteBlock(timeCreation, deferredResponse)

        final case class Success(
            newBody: Block.Body,
            newCommitment: KzgCommitment
        )

        type Error = ledger.JointLedger.CompleteBlockError
    }
}
