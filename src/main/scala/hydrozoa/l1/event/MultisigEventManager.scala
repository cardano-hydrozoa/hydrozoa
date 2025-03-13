package hydrozoa.l1.event

import com.typesafe.scalalogging.Logger
import hydrozoa.*
import hydrozoa.infra.{onlyAddressOutput, outputDatum, txHash, txInputsRef}
import hydrozoa.l1.multisig.state.{MultisigTreasuryDatum, given_FromData_MultisigTreasuryDatum}
import hydrozoa.l2.consensus.HeadParams
import hydrozoa.node.server.{DepositTag, NodeStateManager, SettledDeposit, TreasuryTag}
import scalus.builtin.Data.fromData

/** This class is in charge of handling L1 events.
  *
  * TODO: Apparently we don't need to hanle all L1 transactions. See
  * https://github.com/cardano-hydrozoa/hydrozoa/issues/11
  *
  * @param state
  */
case class MultisigEventManager(
    headParams: HeadParams,
    nativeScript: NativeScript,
    beaconTokenName: String,
    headAddress: AddressBechL1,
    state: NodeStateManager,
    log: Logger
):

    def handleInitTx(initTx: L1Tx, seedAddress: AddressBechL1) =
        val txId = txHash(initTx)
        log.info(s"Handling init tx $txId") // FIXME: perf
        onlyAddressOutput(initTx, headAddress) match
            case Some(ix, coins) =>
                log.info(s"Treasury output index is: $ix");
                state.asAbsent {
                    _.initializeHead(
                      headParams,
                      nativeScript,
                      headAddress,
                      beaconTokenName,
                      mkUtxo[L1, TreasuryTag](txId, ix, headAddress, coins),
                      seedAddress
                    )
                }
            case None =>
                log.error("Can't find treasury in the initialization tx!")

    def handleDepositTx(depositTx: L1Tx, txId: TxId) =
        log.info(s"Handling deposit tx ${txId}")
        // TODO: check the datum
        onlyAddressOutput(depositTx, headAddress) match
            case Some(ix, coins) =>
                log.info(s"Deposit output index is: $ix");
                state.asOpen(_.enqueueDeposit(mkUtxo[L1, DepositTag](txId, ix, headAddress, coins)))
            case None =>
                log.error(
                  "Can't find the deposit output in the deposit tx (should not be the case)!"
                )

    def handleSettlementTx(tx: L1Tx, txHash: TxId) =
        log.info(s"Handling settlement tx $txHash")

        // val treasury = state.asOpen(_.currentTreasuryRef)
        // Inputs of a settlement tx should be either deposits or the treasury
        // val inputs: Set[(TxId, TxIx)] = txInputsRef(tx)
        // val deposits = (inputs - treasury).map((id, ix) => SettledDeposit(id, ix))

        // TODO: Outputs of settlement might be
        //  - withdrawals
        //  - the rollout
        //  - the treasury
        // val newTreasury: MultisigTreasuryDatum = fromData(outputDatum(tx, TxIx(0)))

        val Some(treasury) = onlyAddressOutput(tx, headAddress)
        state.asOpen(_.newTreasury(txHash, TxIx(0), treasury._2))

    def handleFinalizationTx(tx: L1Tx, txHash: TxId) =
        log.info(s"Handling finalization tx: $txHash")
        state.asOpen(_.finalizeHead())
