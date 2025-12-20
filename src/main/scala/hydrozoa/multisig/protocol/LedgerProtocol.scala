package hydrozoa.multisig.protocol

import cats.effect.IO
import cats.syntax.all.*
import com.suprnation.actor.ActorRef.ActorRef
import hydrozoa.lib.actor.SyncRequest
import hydrozoa.multisig.ledger
import hydrozoa.multisig.ledger.dapp.tx.{DepositTx, RefundTx, Tx}
import hydrozoa.multisig.ledger.virtual.GenesisObligation
import hydrozoa.multisig.ledger.virtual.commitment.KzgCommitment.KzgCommitment
import hydrozoa.multisig.protocol.types.Block
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
        txSerialized: ledger.dapp.tx.Tx.Serialized
    ) {
        def ?:(
            actorRef: ActorRef[IO, SyncRequest[
              IO,
              RegisterDeposit,
              Either[RegisterDeposit.Error, RegisterDeposit.Success]
            ]]
        ): IO[Either[RegisterDeposit.Error, RegisterDeposit.Success]] =
            SyncRequest.send(actorRef, this)
    }

    object RegisterDeposit {
        final case class Success(
            genesisObligations: List[GenesisObligation],
            refundTxs: List[RefundTx.PostDated]
        )

        type Error = DepositTx.ParseError
    }

    final case class VirtualTransaction(txSerialized: Tx.Serialized) {
        def ?:(
            actorRef: ActorRef[IO, SyncRequest[
              IO,
              VirtualTransaction,
              Either[VirtualTransaction.Error, VirtualTransaction.Success]
            ]]
        ): IO[Either[VirtualTransaction.Error, VirtualTransaction.Success]] =
            SyncRequest.send(actorRef, this)
    }

    object VirtualTransaction {
        final case class Success(
            payoutObligations: List[Unit] // ledger.JointLedger.PayoutObligation]
        )

        type Error = ledger.VirtualLedger.CborParseError |
            ledger.VirtualLedger.TransactionInvalidError
    }

    final case class CompleteBlock(
        timeCreation: FiniteDuration,
    ) {
        def ?:(
            actorRef: ActorRef[
              IO,
              SyncRequest[IO, CompleteBlock, Either[CompleteBlock.Error, CompleteBlock.Success]]
            ]
        ): IO[Either[CompleteBlock.Error, CompleteBlock.Success]] =
            SyncRequest.send(actorRef, this)
    }

    object CompleteBlock {
        final case class Success(
            newBody: Block.Body,
            newCommitment: KzgCommitment
        )

        type Error = ledger.JointLedger.CompleteBlockError
    }
}
