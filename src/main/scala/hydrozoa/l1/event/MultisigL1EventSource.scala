package hydrozoa.l1.event

import com.typesafe.scalalogging.Logger
import hydrozoa.*
import hydrozoa.infra.*
import hydrozoa.l1.CardanoL1
import hydrozoa.l1.multisig.state.{
    DepositDatum,
    DepositTag,
    TreasuryTag,
    given_FromData_DepositDatum
}
import hydrozoa.node.state.NodeState
import ox.channels.ActorRef
import scalus.builtin.Data.fromData

/** This class is in charge of sourcing L1 events in the multisig regime.
  */
class MultisigL1EventSource(
    nodeState: ActorRef[NodeState],
    cardano: ActorRef[CardanoL1]
):
    private val log = Logger(getClass)

    def awaitInitTx(txId: TxId, headAddress: AddressBechL1): Unit =
        cardano.ask(_.awaitTx(txId)) match
            case Some(initTx) =>
                log.info(s"Init tx $txId appeared on-chain.")
                onlyOutputToAddress(initTx, headAddress) match
                    case Right(ix, coins, _) =>
                        val utxo = mkUtxo[L1, TreasuryTag](txId, ix, headAddress, coins)
                        log.info(s"Treasury utxo index is: $ix, utxo $utxo");
                        nodeState.tell(
                          _.head.initializingPhase(
                            _.openHead(utxo)
                          )
                        )
                    case Left(err) =>
                        err match
                            case _: NoMatch =>
                                throw RuntimeException("Can't find treasury in the init tx!")
                            case _: TooManyMatches =>
                                throw RuntimeException(
                                  "Init tx contains more than one multisig outputs!"
                                )
            case None =>
                throw RuntimeException("initTx hasn't appeared")

//
//    def handleDepositTx(depositTx: TxAny, txId: TxId): Unit =
//        log.info(s"Handling deposit tx ${txId}")
//        // TODO: check the datum
//        // FIXME: don't use onlyOutputToAddress
//        onlyOutputToAddress(depositTx, headAddress) match
//            case Right(ix, coins, datumAsData) =>
//                log.info(s"Deposit output index is: $ix")
//                val datum: DepositDatum = fromData(
//                  datumAsData
//                )
//                // FIXME: This is totally not correct: use deposit datum properly
//                // FIXME how to check soundness of data? // Try(fromData(...))
//                nodeState.head.openPhase(
//                  _.enqueueDeposit(
//                    mkUtxo[L1, DepositTag]( // FIXME
//                      txId,
//                      ix,
//                      extractAddress(datum.address).asL1,
//                      coins
//                    )
//                  )
//                )
//            case Left(err) =>
//                err match
//                    case _: NoMatch => log.error("Can't find the deposit output in the deposit tx!")
//                    case _: TooManyMatches =>
//                        log.error("Deposit tx contains more than one multisig outputs!")
//
//    def handleSettlementTx(tx: TxAny, txHash: TxId): Unit =
//        log.info(s"Handling settlement tx $txHash")
//
//        /** TODO: Outputs of settlement might be
//          *   - withdrawals
//          *   - the rollout
//          *   - the treasury
//          */
//
//        // TODO: handle datum
//        // val newTreasuryDatum: MultisigTreasuryDatum = fromData(outputDatum(tx, TxIx(0)))
//
//        val Right(treasury) = onlyOutputToAddress(tx, headAddress)
//        nodeState.head.openPhase(_.newTreasury(txHash, TxIx(0), treasury._2))
//
//    def handleFinalizationTx(tx: TxAny, txHash: TxId): Unit =
//        log.info(s"Handling finalization tx: $txHash")
