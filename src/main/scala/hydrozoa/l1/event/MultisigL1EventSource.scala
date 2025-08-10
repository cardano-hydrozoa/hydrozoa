package hydrozoa.l1.event

import com.typesafe.scalalogging.Logger
import hydrozoa.*
import hydrozoa.infra.*
import hydrozoa.infra.transitionary.toScalus
import hydrozoa.l1.CardanoL1
import hydrozoa.l1.multisig.state.*
import hydrozoa.node.state.HeadPhase.{Finalizing, Open}
import hydrozoa.node.state.NodeState
import ox.channels.ActorRef
import ox.scheduling.{RepeatConfig, repeat}
import ox.{fork, sleep, supervised}
import scalus.bloxbean.Interop
import scalus.builtin.ByteString
import scalus.builtin.Data.fromData
import scalus.cardano.address.{ShelleyAddress, Address as SAddress}
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.Script.Native
import scalus.cardano.ledger.{
    AssetName,
    Blake2b_256,
    Coin,
    Hash,
    HashPurpose,
    MultiAsset,
    TransactionHash,
    TransactionInput,
    TransactionOutput,
    Value
}

import java.math.BigInteger
import scala.collection.immutable.SortedMap
import scala.collection.mutable
import scala.concurrent.duration.DurationInt
import scala.jdk.CollectionConverters.*
import scala.language.strictEquality
import scala.util.Try

/** This class is in charge of sourcing L1 events in the multisig regime.
  *
  * TODO: L1 source component ideally should use streaming not polling
  */
class MultisigL1EventSource(
    nodeState: ActorRef[NodeState],
    cardano: ActorRef[CardanoL1]
):
    private val log = Logger(getClass)

    def awaitInitTx(
        txId: TransactionHash,
        headAddress: AddressL1,
        headNativeScript: Native,
        beaconTokenName: AssetName
    ): Unit =
        log.info("awaitInitTx")
        cardano.ask(_.awaitTx(txId)) match
            case Some(initTx) =>
                log.info(s"Init tx $txId appeared on-chain.")
                onlyOutputToAddress(initTx, headAddress) match
                    case Right(ix, value, _) =>
                        val utxo = TaggedUtxo[L1, TreasuryTag](Utxo(txId, ix, headAddress, value))
                        log.info(s"Treasury utxo index is: $ix, utxo $utxo");
                        nodeState.tell(s =>
                            s.head.initializingPhase(
                              _.openHead(utxo)
                            )
                        )

                        val treasuryTokenAmount =
                            MultiAsset(
                              SortedMap(
                                headNativeScript.scriptHash ->
                                    SortedMap(beaconTokenName -> 1L)
                              )
                            )

                        // repeat polling for head address while head exists
                        supervised {
                            fork {
                                repeat(RepeatConfig.fixedRateForever(1000.millis))(
                                  updateL1State(headAddress, treasuryTokenAmount)
                                )
                            }

                            var loop = true
                            while loop do
                                sleep(100.millis)
                                loop = nodeState.ask(_.mbInitializedOn.isDefined)
                                    && (
                                      nodeState.ask(_.head.currentPhase == Open)
                                          || nodeState.ask(_.head.currentPhase == Finalizing)
                                    )

                            log.info("Leaving L1 source supervised scope")
                        }
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
        case Treasury(utxo: Utxo[L1])
        case Deposit(utxo: Utxo[L1])
        case Unknown(utxo: Utxo[L1])

    private def utxoType(treasuryTokenAmount: MultiAsset)(utxo: Utxo[L1]): MultisigUtxoType = {
        // TODO: Multiasset needs a (type safe) `contains` method
        val treasuryTokenId = treasuryTokenAmount.assets.toList.head._1
        val treasuryTokenName = treasuryTokenAmount.assets(treasuryTokenId).toList.head._1
        val utxoAssets = utxo.output.value.assets.assets
        if utxoAssets.contains(treasuryTokenId) && utxoAssets(treasuryTokenId).contains(
              treasuryTokenName
            ) && utxoAssets(treasuryTokenId)(treasuryTokenName) == 1
        then MultisigUtxoType.Treasury(utxo)
        else
            // FIXME: use depositDatum function
            val datum = Try(
              fromData[DepositDatum](utxo.output.datumOption.get.asInstanceOf[Inline].data)
            ).toOption

            datum match
                case Some(_) => MultisigUtxoType.Deposit(utxo)
                case None    => MultisigUtxoType.Unknown(utxo)
    }

    private def mkNewTreasuryUtxo(utxo: Utxo[L1]): TreasuryUtxo = {
        TaggedUtxo[L1, TreasuryTag](utxo)
    }

    /** Doesn't check the datum, use only when you are sure it's a deposit utxo.
      */
    private def mkDepositUtxoUnsafe(utxo: Utxo[L1]): DepositUtxo = {
        TaggedUtxo[L1, DepositTag](utxo)
    }

    private def updateL1State(
        headAddress: AddressL1,
        treasuryTokenAmount: MultiAsset
    ): Unit =
        nodeState.ask(_.mbInitializedOn) match
            case None =>
                log.trace("Head does not exist...")
            case Some(_) =>
                nodeState.ask(_.head.currentPhase) match
                    case Open =>
                        log.trace("Polling head address, phase=Open...")
                        val utxos = cardano.ask(_.utxosAtAddress(headAddress))
                        // FIXME: no guarantees head is still open
                        val currentL1State = nodeState.ask(_.head.openPhase(_.stateL1))
                        // beacon token -> treasury
                        // rollout token -> rollout
                        // otherwise -> maybe deposit
                        val (mbNewTreasury, depositsNew, depositsGone) =
                            //
                            var mbNewTreasury: Option[TreasuryUtxo] = None
                            val depositsNew: TaggedUtxoSetMutable[L1, DepositTag] =
                                TaggedUtxoSetMutable[L1, DepositTag](mutable.Map.empty)
                            //
                            val knownDepositIds = currentL1State.depositUtxos.utxoMap.keySet
                            val existingDeposits: mutable.Set[UtxoIdL1] = mutable.Set.empty

                            val utxoType_ = utxoType(treasuryTokenAmount)

                            utxos.foreach(utxo =>
                                val utxoId = utxo.input
                                utxoType_(utxo) match
                                    case MultisigUtxoType.Treasury(utxo) =>
                                        // log.debug(s"UTXO type: treasury $utxoId")
                                        // FIXME: TransactionHash and TransactionHash can't be compared with == (????)
                                        if currentL1State.treasuryUtxo.untagged.input.index == utxoId.index &&
                                            currentL1State.treasuryUtxo.untagged.input.transactionId.toHex == utxoId.transactionId.toHex
                                        then mbNewTreasury = Some(mkNewTreasuryUtxo(utxo))
                                    case MultisigUtxoType.Deposit(utxo) =>
                                        // log.debug(s"UTXO type: deposit $utxoId")
                                        existingDeposits.add(utxoId)
                                        if (!knownDepositIds.contains(utxoId)) then
                                            val depositUtxo = mkDepositUtxoUnsafe(utxo)
                                            depositsNew.utxoMap
                                                .put(
                                                  depositUtxo.untagged.input,
                                                  depositUtxo.untagged.output
                                                )
                                    case MultisigUtxoType.Unknown(utxo) =>
                                        log.debug(s"UTXO type: unknown: $utxoId")
                            )
                            val depositsGone: Set[UtxoIdL1] =
                                knownDepositIds.toSet &~ existingDeposits.toSet
                            (mbNewTreasury, depositsNew, depositsGone)

                        // FIXME: no guarantees head is still open
                        nodeState.tell(_.head.openPhase(openHead =>
                            mbNewTreasury.foreach(openHead.setNewTreasuryUtxo)
                            if depositsGone.nonEmpty then openHead.removeDepositUtxos(depositsGone)
                            if depositsNew.utxoMap.nonEmpty then
                                openHead.addDepositUtxos(depositsNew |> TaggedUtxoSet.apply)
                        ))
                    case Finalizing =>
                        log.trace("Polling head address, phase=Finalizing...")
                        val utxos = cardano.ask(_.utxosAtAddress(headAddress))
                        // FIXME: no guarantees head is still finalizing
                        val currentL1State = nodeState.ask(_.head.finalizingPhase(_.stateL1))
                        // beacon token -> treasury
                        // rollout token -> rollout
                        // otherwise -> maybe deposit
                        val mbNewTreasury =
                            //
                            var mbNewTreasury: Option[TreasuryUtxo] = None
                            val utxoType_ = utxoType(treasuryTokenAmount)

                            utxos.foreach(utxo =>
                                val utxoId = utxo.input
                                utxoType_(utxo) match
                                    case MultisigUtxoType.Treasury(utxo) =>
                                        // log.debug(s"UTXO type: treasury $utxoId")
                                        if currentL1State.treasuryUtxo.untagged.input != utxoId then
                                            mbNewTreasury = Some(mkNewTreasuryUtxo(utxo))
                                    case MultisigUtxoType.Unknown(utxo) =>
                                        log.debug(s"UTXO type: unknown: $utxoId")
                                    case other =>
                                        log.info(
                                          s"UTxO of type $other are ignored in Finalizing mode"
                                        )
                            )
                            mbNewTreasury

                        // FIXME: no guarantees head is still finalizing
                        nodeState.tell(
                          _.head.finalizingPhase(finalizingHead =>
                              mbNewTreasury.foreach(finalizingHead.newTreasuryUtxo)
                          )
                        )
                    case other => log.warn(s"Unsupported phase: $other")
