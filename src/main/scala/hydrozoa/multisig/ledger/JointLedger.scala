package hydrozoa.multisig.ledger

import cats.effect.{IO, Ref}
import hydrozoa.multisig.ledger.DappLedger.ErrorAddDeposit
import hydrozoa.multisig.ledger.VirtualLedger.{ErrorApplyInternalTx, ErrorApplyWithdrawalTx}
import hydrozoa.multisig.ledger.dapp.tx.DepositTx
import hydrozoa.multisig.ledger.dapp.utxo.DepositUtxo
import hydrozoa.multisig.ledger.virtual.tx.{InternalTx, WithdrawalTx}
import scalus.cardano.ledger.*

// TODO: Should be an actor. See VirtualLedger
// NOTE: Joint ledger is created by the MultisigManager.
// NOTE: As of 2025-11-16, George says BlockWeaver should be the ONLY actor calling the joint ledger
final case class JointLedger()(
    private val dappLedger: DappLedger, // TODO: Make actor ref
    private val virtualLedger: VirtualLedger, // TODO: Make actor ref

    // TODO: these are payout obligations, populated by synchronous calls to "applyTx" in the virtual ledger.
    // They are discharged during block settlement when we call "settleLedger" in the dapp ledger
    private val blockWithdrawnUtxos: Ref[IO, List[TransactionOutput]]
    // TODO: private val state : Block,
    // type State = Done(producedBlock: Block)
    //   | Producing( previousBlock: Block, startTime : PosixTime, nextBlockData : TransientFields)
    // TODO: We need a set of transient fields that are needed during production of a block and
    //   - are complied into a finished block when completeBlock is called (which then gets put into "previous block")
    //   - get wiped when complete block is called
    //   - "transient fields" can probably be another type extended from BlockBody fields
) {
    // TODO remane: registerDeposit (all deposits are L1)
    // TODO: should take same argument as DappLedger's register deposit (deposit + refund tx seq)
    // TODO: Should return IO[Unit]
    // Comment: this might be redundant, but probably not -- this joint ledger is the only one that's
    // aware of blocks. We should error if we are not currently producing a block
    def registerDepositL1(tx: DepositTx): IO[Either[ErrorAddDeposit, DepositUtxo]] = {
        // - Update the current block with the accepted/rejected decision from the dApp ledger
        // - update the block with ledgerEventsRequired
        ???
    }

    // TODO: Collapse this with applyWithdrawalTxL2
    // TODO: Update the current block with the result of passing the tx to the virtual ledger, as well as updating
    // ledgerEventsRequired
    def applyInternalTxL2(tx: InternalTx): IO[Either[ErrorApplyInternalTx, Unit]] =
        ???

    def applyWithdrawalTxL2(tx: WithdrawalTx): IO[Either[ErrorApplyWithdrawalTx, Unit]] =
        ???

    // TODO: more methods related to block completion
    // def: "augmented block" == "block plus all of its l1 effects (settlementTxSeq, etc.)"
    // when a block is finished, we:
    //   - send Aug block to block signer along with the L1 effects settlementTxSeq, etc. for signing locally
    //     (signatures subsequently passed to peer liason for circulation and block weaver and to the )
    //   - sends block itself to peer liason for circulation
    //   - sends just L1 effects to cardano liason

    // TODO: def startBlock(blockCreationTime : PosixTime): IO[Unit]
    // - Set the creation time (need this for the exact time stamp against which we validate deposit times)
    //

    // Block completion Signal is provided to the joint ledger when the block weaver says its time.
    // If its a final block, we don't pass poll results from the cardano liason. Otherwise we do.
    // We need to:
    //   - Compile the information from the transient fields into a block
    //   - put it into "previous block"
    //   - wipe the "transient fields"
    // If a "reference block" is passed, this means that the block we produce must be equal to the reference block.
    // If the produced block is NOT equal to a passed reference block, then:
    //   - Consensus is broken
    //   - Send a panic to the multisig regime manager in a suicide note
    // TODO: def completeBlockRegular(pollResults, referenceBlock : Option[Block]) : IO[Unit]
    // TODO: def completeBlockFinal(referenceBlock : Option[Block]) : IO[Unit]
    //

}

/** ==Hydrozoa's joint ledger on Cardano in the multisig regime==
  *
  * Hydrozoa's joint ledger connects its dapp ledger to its virtual ledger. It dispatches some state
  * transitions to them individually, but it also periodically reconciles state transitions across
  * them to keep them aligned.
  */
object JointLedger {
    final case class CompleteBlockError() extends Throwable
}
