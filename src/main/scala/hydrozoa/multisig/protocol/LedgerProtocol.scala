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
        def ?: : SyncRequest.SendE[
          IO,
          RegisterDeposit,
          RegisterDeposit.Error,
          RegisterDeposit.Success
        ] = SyncRequest.send(_, this)
    }

    object RegisterDeposit {
        final case class Success(
            genesisObligations: List[GenesisObligation],
            refundTxs: List[RefundTx.PostDated]
        )

        type Error = DepositTx.ParseError
    }

    final case class VirtualTransaction(txSerialized: Tx.Serialized) {
        def ?: : SyncRequest.SendE[
          IO,
          VirtualTransaction,
          VirtualTransaction.Error,
          VirtualTransaction.Success
        ] = SyncRequest.send(_, this)
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
        def ?: : SyncRequest.SendE[IO, CompleteBlock, CompleteBlock.Error, CompleteBlock.Success] =
            SyncRequest.send(_, this)
    }

    object CompleteBlock {
        final case class Success(
            newBody: Block.Body,
            newCommitment: KzgCommitment
        )

        type Error = ledger.JointLedger.CompleteBlockError
    }
}
