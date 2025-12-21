package hydrozoa.multisig.protocol

import cats.effect.IO
import cats.syntax.all.*
import com.suprnation.actor.ActorRef.ActorRef
import hydrozoa.lib.actor.{SyncRequest, SyncRequestE}
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
        type Request = RegisterDeposit.Sync
    }

    object VirtualLedger {
        type VirtualLedgerRef = Ref
        type Ref = ActorRef[IO, Request]
        type Request = VirtualTransaction.Sync
    }

    object JointLedger {
        type JointLedgerRef = Ref
        type Ref = ActorRef[IO, Request]
        type Request = RegisterDeposit.Sync | VirtualTransaction.Sync | CompleteBlock.Sync
    }

    final case class RegisterDeposit(
        txSerialized: ledger.dapp.tx.Tx.Serialized
    ) extends SyncRequestE[IO, RegisterDeposit, RegisterDeposit.Error, RegisterDeposit.Success] {
        export RegisterDeposit.Sync
        def ?: : this.Send = SyncRequest.send(_, this)
    }

    object RegisterDeposit {
        type Sync = SyncRequest.EnvelopeE[
          IO,
          RegisterDeposit,
          RegisterDeposit.Error,
          RegisterDeposit.Success
        ]

        final case class Success(
            genesisObligations: List[GenesisObligation],
            refundTxs: List[RefundTx.PostDated]
        )

        type Error = DepositTx.ParseError
    }

    final case class VirtualTransaction(txSerialized: Tx.Serialized)
        extends SyncRequestE[
          IO,
          VirtualTransaction,
          VirtualTransaction.Error,
          VirtualTransaction.Success
        ] {
        export VirtualTransaction.Sync
        def ?: : this.Send = SyncRequest.send(_, this)
    }

    object VirtualTransaction {
        type Sync = SyncRequest.EnvelopeE[
          IO,
          VirtualTransaction,
          VirtualTransaction.Error,
          VirtualTransaction.Success
        ]

        final case class Success(
            payoutObligations: List[Unit] // ledger.JointLedger.PayoutObligation]
        )

        type Error = ledger.VirtualLedger.CborParseError |
            ledger.VirtualLedger.TransactionInvalidError
    }

    final case class CompleteBlock(
        timeCreation: FiniteDuration,
    ) extends SyncRequestE[IO, CompleteBlock, CompleteBlock.Error, CompleteBlock.Success] {
        export CompleteBlock.Sync
        def ?: : this.Send = SyncRequest.send(_, this)
    }

    object CompleteBlock {
        type Sync =
            SyncRequest.EnvelopeE[IO, CompleteBlock, CompleteBlock.Error, CompleteBlock.Success]

        final case class Success(
            newBody: Block.Body,
            newCommitment: KzgCommitment
        )

        type Error = ledger.JointLedger.CompleteBlockError
    }
}
