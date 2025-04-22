package hydrozoa.l1.event

import com.bloxbean.cardano.client.api.model.Amount.asset
import com.bloxbean.cardano.client.api.model.{Amount, Utxo as BBUtxo}
import com.bloxbean.cardano.client.transaction.spec.script.NativeScript as BBNativeScript
import com.typesafe.scalalogging.Logger
import hydrozoa.*
import com.typesafe.scalalogging.Logger
import hydrozoa.infra.*
import hydrozoa.*
import hydrozoa.l1.CardanoL1
import hydrozoa.infra.*
import hydrozoa.l1.multisig.state.{DepositTag, DepositUtxo, TreasuryTag, TreasuryUtxo}
import hydrozoa.l1.CardanoL1
import hydrozoa.node.state.NodeState
import hydrozoa.l1.multisig.state.{DepositTag, DepositUtxo, TreasuryTag, TreasuryUtxo}
import ox.channels.ActorRef
import hydrozoa.node.state.NodeState
import ox.scheduling.{RepeatConfig, repeat}
import ox.channels.ActorRef

import java.math.BigInteger
import ox.scheduling.{RepeatConfig, repeat}

import scala.jdk.CollectionConverters.*

import scala.collection.mutable
import java.math.BigInteger
import scala.concurrent.duration.DurationInt
import scala.collection.mutable
import scala.language.strictEquality

/** This class is in charge of sourcing L1 events in the multisig regime.
  */
class MultisigL1EventSource(
    nodeState: ActorRef[NodeState],
    cardano: ActorRef[CardanoL1]
):
    private val log = Logger(getClass)

    def awaitInitTx(
        txId: TxId,
        headAddress: AddressBechL1,
        headNativeScript: NativeScript,
        beaconTokenName: TokenName
    ): Unit =
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
                        // repeat polling for head address forever
                        val nativeScriptBB =
                            BBNativeScript.deserializeScriptRef(headNativeScript.bytes)
                        val treasuryTokenAmount = asset(
                          nativeScriptBB.getPolicyId,
                          beaconTokenName.tokenName,
                          BigInteger.valueOf(1)
                        )
                        // FIXME: stop once head is closed
                        // FIXME: repeat config
                        // TODO: use streaming
                        repeat(RepeatConfig.fixedRateForever(500.millis))(
                          updateL1State(headAddress, treasuryTokenAmount)
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

    private enum MultisigUtxoType:
        case Treasury(utxo: BBUtxo)
        case Deposit(utxo: BBUtxo)

    private def utxoType(treasuryTokenAmount: Amount)(utxo: BBUtxo): MultisigUtxoType =
        if utxo.getAmount.contains(treasuryTokenAmount)
        then MultisigUtxoType.Treasury(utxo)
        else MultisigUtxoType.Deposit(utxo)

    private def mkNewTreasuryUtxo(utxo: BBUtxo): TreasuryUtxo =
        val utxoId = UtxoId[L1]
            .apply(utxo.getTxHash |> TxId.apply, utxo.getOutputIndex |> TxIx.apply)
        val coins = utxo.getAmount.asScala.find(_.getUnit.equals("lovelace")).get.getQuantity
        mkUtxo[L1, TreasuryTag](
          utxoId.txId,
          utxoId.outputIx,
          utxo.getAddress |> AddressBechL1.apply,
          coins
        )

    private def mkDepositUtxo(utxo: BBUtxo): DepositUtxo =
        val utxoId = UtxoId[L1]
            .apply(utxo.getTxHash |> TxId.apply, utxo.getOutputIndex |> TxIx.apply)
        val coins = utxo.getAmount.asScala.find(_.getUnit.equals("lovelace")).get.getQuantity
        mkUtxo[L1, DepositTag](
          utxoId.txId,
          utxoId.outputIx,
          utxo.getAddress |> AddressBechL1.apply,
          coins
        )

    private def updateL1State(
        headAddress: AddressBechL1,
        treasuryTokenAmount: Amount
    ): Unit =
        log.trace("Polling head address...")
        val utxos = cardano.ask(_.utxosAtAddress(headAddress))
        val currentL1State = nodeState.ask(_.head.openPhase(_.stateL1))
        // beacon token -> treasury
        // rollout token -> rollout
        // otherwise -> maybe deposit
        val (mbNewTreasury, depositsNew, depositsGone) =
            //
            var mbNewTreasury: Option[TreasuryUtxo] = None
            val depositsNew: UtxoSetMutable[L1, DepositTag] =
                UtxoSetMutable[L1, DepositTag](mutable.Map.empty)
            //
            val knownDepositIds = currentL1State.depositUtxos.map.keySet
            val existingDeposits: mutable.Set[UtxoIdL1] = mutable.Set.empty

            val utxoType_ = utxoType(treasuryTokenAmount)

            utxos.foreach(utxo =>
                val utxoId = UtxoId[L1]
                    .apply(utxo.getTxHash |> TxId.apply, utxo.getOutputIndex |> TxIx.apply)
                utxoType_(utxo) match
                    case MultisigUtxoType.Treasury(utxo) =>
                        if currentL1State.treasuryUtxo.ref != utxoId then
                            mbNewTreasury = Some(mkNewTreasuryUtxo(utxo))
                    case MultisigUtxoType.Deposit(utxo) =>
                        existingDeposits.add(utxoId)
                        if (!knownDepositIds.contains(utxoId)) then
                            val depositUtxo = mkDepositUtxo(utxo)
                            depositsNew.map.put(depositUtxo.ref, depositUtxo.output)
            )
            val depositsGone: Set[UtxoIdL1] = knownDepositIds.toSet &~ existingDeposits.toSet
            (mbNewTreasury, depositsNew, depositsGone)

        nodeState.tell(_.head.openPhase(openHead =>
            mbNewTreasury.foreach(openHead.newTreasuryUtxo)
            if depositsGone.nonEmpty then openHead.removeDepositUtxos(depositsGone)
            if depositsNew.map.nonEmpty then openHead.addDepositUtxos(depositsNew |> UtxoSet.apply)
        ))

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
