package hydrozoa.l1.event

import com.typesafe.scalalogging.Logger
import hydrozoa.infra.{onlyAddressOutput, txHash}
import hydrozoa.l2.consensus.HeadParams
import hydrozoa.node.server.{AwaitingDeposit, HeadStateManager}
import hydrozoa.{AddressBechL1, L1Tx, NativeScript, TxId}

/** This class is in charge of handling L1 events.
  *
  * @param headStateManager
  */
case class MultisigEventManager(
    headParams: HeadParams,
    nativeScript: NativeScript,
    headAddress: AddressBechL1,
    headStateManager: HeadStateManager,
    log: Logger
):

    def handleInitTx(initTx: L1Tx) =
        val txId = txHash(initTx)
        log.info(s"Handling init tx $txId") // FIXME: perf
        onlyAddressOutput(initTx, headAddress) match
            case Some(ix) =>
                log.info(s"Treasury output index is: $ix");
                headStateManager.init(headParams, nativeScript, headAddress, (txId, ix))
            case None =>
                log.error("Can't find treasury in the initialization tx!")

    def handleDepositTx(depositTx: L1Tx, txHash: TxId) =
        log.info(s"Handling deposit tx ${txHash}")
        // TODO: check the datum
        onlyAddressOutput(depositTx, headAddress) match
            case Some(ix) =>
                log.info(s"Deposit output index is: $ix");
                headStateManager.enqueueDeposit(AwaitingDeposit(txHash, ix))
            case None =>
                log.error(
                  "Can't find the deposit output in the deposit tx (should not be the case)!"
                )

    def handleSettlementTx() =
        log.info(s"Handling settlement tx...")
        ???
