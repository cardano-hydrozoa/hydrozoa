package hydrozoa.l1.event

import com.typesafe.scalalogging.Logger
import hydrozoa.*
import hydrozoa.infra.*
import hydrozoa.l1.multisig.state.{
    DepositDatum,
    DepositTag,
    TreasuryTag,
    given_FromData_DepositDatum
}
import hydrozoa.l2.consensus.HeadParams
import hydrozoa.node.state.NodeState
import scalus.builtin.Data.fromData

/** This class is in charge of handling L1 events.
  *
  * TODO: Apparently we don't need to handle all L1 transactions. See
  * https://github.com/cardano-hydrozoa/hydrozoa/issues/11
  */
case class MultisigL1EventManager(
    headParams: HeadParams,
    nativeScript: NativeScript,
    beaconTokenName: String,
    headAddress: AddressBechL1,
    nodeState: NodeState,
    log: Logger
):

    def handleInitTx(initTx: TxAny, seedAddress: AddressBechL1): Unit =
        val txId = txHash(initTx)
        log.info(s"Handling init tx $txId")
        onlyOutputToAddress(initTx, headAddress) match
            case Right(ix, coins, _) =>
                log.info(s"Treasury output index is: $ix");
                nodeState.head.initializingPhase {
                    _.openHead(
                      headParams,
                      nativeScript,
                      headAddress,
                      beaconTokenName,
                      mkUtxo[L1, TreasuryTag](txId, ix, headAddress, coins),
                      seedAddress
                    )
                }
            case Left(err) =>
                err match
                    case _: NoMatch => log.error("Can't find treasury in the initialization tx!")
                    case _: TooManyMatches =>
                        log.error("Initialization tx contains more than one multisig outputs!")

    def handleDepositTx(depositTx: TxAny, txId: TxId): Unit =
        log.info(s"Handling deposit tx ${txId}")
        // TODO: check the datum
        // FIXME: don't use onlyOutputToAddress
        onlyOutputToAddress(depositTx, headAddress) match
            case Right(ix, coins, datumAsData) =>
                log.info(s"Deposit output index is: $ix")
                val datum: DepositDatum = fromData(
                  datumAsData
                ) 
                // FIXME: This is totally not correct: use deposit datum properly
                // FIXME how to check soundness of data? // Try(fromData(...))
                nodeState.head.openPhase(
                  _.enqueueDeposit(
                    mkUtxo[L1, DepositTag]( // FIXME
                      txId,
                      ix,
                      extractAddress(datum.address).asL1,
                      coins
                    )
                  )
                )
            case Left(err) =>
                err match
                    case _: NoMatch => log.error("Can't find the deposit output in the deposit tx!")
                    case _: TooManyMatches =>
                        log.error("Deposit tx contains more than one multisig outputs!")

    def handleSettlementTx(tx: TxAny, txHash: TxId): Unit =
        log.info(s"Handling settlement tx $txHash")

        /** TODO: Outputs of settlement might be
          *   - withdrawals
          *   - the rollout
          *   - the treasury
          */

        // TODO: handle datum
        // val newTreasuryDatum: MultisigTreasuryDatum = fromData(outputDatum(tx, TxIx(0)))

        val Right(treasury) = onlyOutputToAddress(tx, headAddress)
        nodeState.head.openPhase(_.newTreasury(txHash, TxIx(0), treasury._2))

    def handleFinalizationTx(tx: TxAny, txHash: TxId): Unit =
        log.info(s"Handling finalization tx: $txHash")
