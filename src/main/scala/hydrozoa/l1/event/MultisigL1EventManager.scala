package hydrozoa.l1.event

import com.typesafe.scalalogging.Logger
import hydrozoa.*
import hydrozoa.infra.{NoMatch, TooManyMatches, onlyOutputToAddress, txHash}
import hydrozoa.l1.multisig.state.{DepositTag, TreasuryTag}
import hydrozoa.l2.consensus.HeadParams
import hydrozoa.node.server.NodeStateManager

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
    state: NodeStateManager,
    log: Logger
):

    def handleInitTx(initTx: TxAny, seedAddress: AddressBechL1) =
        val txId = txHash(initTx)
        log.info(s"Handling init tx $txId") // TODO: perf
        onlyOutputToAddress(initTx, headAddress) match
            case Right(ix, coins) =>
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
            case Left(err) =>
                err match
                    case _: NoMatch => log.error("Can't find treasury in the initialization tx!")
                    case _: TooManyMatches =>
                        log.error("Initialization tx contains more than one multisig outputs!")

    def handleDepositTx(depositTx: TxAny, txId: TxId) =
        log.info(s"Handling deposit tx ${txId}")
        // TODO: check the datum
        onlyOutputToAddress(depositTx, headAddress) match
            case Right(ix, coins) =>
                log.info(s"Deposit output index is: $ix");
                state.asOpen(_.enqueueDeposit(mkUtxo[L1, DepositTag](txId, ix, headAddress, coins)))
            case Left(err) =>
                err match
                    case _: NoMatch => log.error("Can't find the deposit output in the deposit tx!")
                    case _: TooManyMatches =>
                        log.error("deposit tx contains more than one multisig outputs!")

    def handleSettlementTx(tx: TxAny, txHash: TxId) =
        log.info(s"Handling settlement tx $txHash")

        /** TODO: Outputs of settlement might be
          *   - withdrawals
          *   - the rollout
          *   - the treasury
          */

        // TODO: handle datum
        // val newTreasuryDatum: MultisigTreasuryDatum = fromData(outputDatum(tx, TxIx(0)))

        val Right(treasury) = onlyOutputToAddress(tx, headAddress)
        state.asOpen(_.newTreasury(txHash, TxIx(0), treasury._2))

    def handleFinalizationTx(tx: TxAny, txHash: TxId) =
        log.info(s"Handling finalization tx: $txHash")
        state.asOpen(_.finalizeHead())
