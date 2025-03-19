package hydrozoa.l2.ledger

import hydrozoa.*
import hydrozoa.infra.*
import hydrozoa.l1.multisig.state.DepositUtxos
import hydrozoa.l2.ledger.event.*
import hydrozoa.l2.ledger.state.*

import scala.collection.mutable
import scala.jdk.CollectionConverters.*

sealed trait TInstancePurpose
sealed trait THydrozoaHead extends TInstancePurpose
sealed trait TBlockProduction extends TInstancePurpose

type UtxosDiff = Set[(OutputRefL2, Output[L2])]
type UtxosDiffMutable = mutable.Set[(OutputRefL2, Output[L2])] // FIXME L2

// FIXME: move InstancePurpose to L2Ledger
case class AdaSimpleLedger[InstancePurpose <: TInstancePurpose] private (
    verifier: Verifier[L2Event]
) extends L2Ledger[
      UtxosSetOpaqueMutable,
      SimpleGenesis,
      SimpleTransaction,
      SimpleWithdrawal,
      UtxosDiff,
      L2Event,
      L2EventHash,
      Verifier[L2Event]
    ]:

    val activeState: UtxosSetOpaqueMutable = mutable.Map.empty

    /** Makes a copy of the current ledger for block production purposes.
      * @param ev
      *   evidence that we are cloning Hydrozoa ledger, not its clone
      * @return
      *   cloned ledger
      */
    def blockProduction(implicit
        ev: InstancePurpose =:= THydrozoaHead
    ): AdaSimpleLedger[TBlockProduction] =
        val ledgerForBlockProduction: AdaSimpleLedger[TBlockProduction] = copy()
        ledgerForBlockProduction.updateUtxosActive(activeState.clone().toMap)
        ledgerForBlockProduction

    override def submit[E1 <: L2Event](
        event: E1
    ): Either[(L2EventHash, String), (L2EventHash, event.UtxosDiff)] =
        require(verifier.isValid(event), true)
        event match
            case genesis: L2Genesis       => handleGenesis(genesis)
            case tx: L2Transaction        => handleTransaction(tx)
            case withdrawal: L2Withdrawal => handleWithdrawal(withdrawal)

    private def handleGenesis(event: L2Genesis) =
        val (_, txId) = AdaSimpleLedger.adopt(event.genesis)

        val utxoDiff = event.genesis.outputs.zipWithIndex
            .map(output =>
                val txIn = OutputRefL2(txId, TxIx(output._2))
                val txOut = Output[L2](output._1.address.asL1, output._1.coins)
                (txIn, txOut)
            )
            .toSet

        val utxoDiffInt = event.genesis.outputs.zipWithIndex
            .map(output =>
                val txIn = liftOutputRef(OutputRefL2(txId, TxIx(output._2)))
                val txOut = liftOutput(output._1.address, output._1.coins)
                (txIn, txOut)
            )
            .toSet

        activeState.addAll(utxoDiffInt)
        Right((txId, utxoDiff))

    private def handleTransaction(event: L2Transaction) =
        val (_, txId) = AdaSimpleLedger.adopt(event.transaction)

        resolveInputs(event.transaction.inputs) match
            case Left(extraneous) =>
                Left(txId, s"Extraneous utxos in transaction $txId: $extraneous")
            case Right(oldUtxos) =>
                // Outputs
                val newUtxos = event.transaction.outputs.zipWithIndex.map(output =>
                    val txIn = liftOutputRef(OutputRefL2(txId, TxIx(output._2)))
                    val txOut = liftOutput(output._1.address, output._1.coins)
                    (txIn, txOut)
                )

                val (inputRefs, inputs, _) = oldUtxos.unzip3

                if !checkSumInvariant(inputs, newUtxos.map(_._2)) then
                    Left(txId, s"Sum invariant is not hold for tx $txId")
                else
                    // FIXME: atomicity
                    inputRefs.foreach(activeState.remove)
                    newUtxos.foreach(activeState.put.tupled)

                    Right((txId, Set[(OutputRefL2, Output[L2])]()))

    private def handleWithdrawal(event: L2Withdrawal) =
        val (_, txId) = AdaSimpleLedger.adopt(event.withdrawal)

        resolveInputs(event.withdrawal.inputs) match
            case Left(extraneous) => Left(txId, s"Extraneous utxos in withdrawal: $extraneous")
            case Right(resolved) =>
                val (withdrawnRefs, withdrawnOutputs, (withdrawnPub)) = resolved.unzip3
                val (withdrawnRefsPub, withdrawnOutputsPub) = withdrawnPub.unzip
                // FIXME: atomicity
                withdrawnRefs.foreach(activeState.remove)
                Right(txId, withdrawnRefsPub.zip(withdrawnOutputsPub).toSet)

    /** Tries to resolve output refs.
      *
      * @param inputs
      *   output refs to resolve
      * @return
      *   Left if
      */
    private def resolveInputs(
        inputs: List[OutputRefL2]
    ): Either[List[OutputRefL2], List[(OutputRefInt, OutputInt, (OutputRefL2, Output[L2]))]] =
        inputs
            .map { e =>
                val outputRefInt = liftOutputRef(e)
                activeState.get(outputRefInt) match
                    case Some(output) => Right(outputRefInt, output, (e, unliftOutput(output)))
                    case None         => Left(e)
            }
            .partitionMap(identity) match
            case (Nil, resolved) => Right(resolved)
            case (extraneous, _) => Left(extraneous)

    override def isEmpty: Boolean = activeState.isEmpty

    override def flush: UtxosDiff =
        val ret = activeState.clone()
        activeState.clear()
        ret.toSet.map((k, v) => (unliftOutputRef(k), unliftOutput(v)))

    override def updateUtxosActive(activeState: UtxosSetOpaque): Unit =
        // TODO: revise
        this.activeState.clear()
        this.activeState.addAll(activeState)

object AdaSimpleLedger:
    def apply(): AdaSimpleLedger[THydrozoaHead] = AdaSimpleLedger[THydrozoaHead](NoopVerifier)

    def adopt(event: SimpleGenesis | SimpleTransaction | SimpleWithdrawal): (TxL2, L2EventHash) =
        event match
            case genesis: SimpleGenesis =>
                val cardanoTx = mkCardanoTxForL2Genesis(genesis)
                val txId = txHash(cardanoTx)
                println(s"L2 genesis event, txId: $txId, content: ${serializeTxHex(cardanoTx)}")
                (cardanoTx, txId)
            case transaction: SimpleTransaction =>
                val cardanoTx = mkCardanoTxForL2Transaction(transaction)
                val txId = txHash(cardanoTx)
                println(s"L2 transaction event, txId: $txId, content: ${serializeTxHex(cardanoTx)}")
                (cardanoTx, txId)
            case withdrawal: SimpleWithdrawal =>
                val cardanoTx = mkCardanoTxForL2Withdrawal(withdrawal)
                val txId = txHash(cardanoTx)
                println(s"L2 withdrawal event, txId: $txId, content: ${serializeTxHex(cardanoTx)}")
                (cardanoTx, txId)

    def mkGenesisEvent(genesis: SimpleGenesis): L2Genesis =
        val (_, txId) = adopt(genesis)
        GenesisL2Event(txId, genesis)

    def mkTransactionEvent(tx: SimpleTransaction): L2Transaction =
        val (_, txId) = adopt(tx)
        TransactionL2Event(txId, tx)

    def mkWithdrawalEvent(withdrawal: SimpleWithdrawal): L2Withdrawal =
        val (_, txId) = adopt(withdrawal)
        WithdrawalL2Event(txId, withdrawal)

case class SimpleGenesis(
    outputs: List[SimpleOutput]
)

object SimpleGenesis:
    def apply(ds: DepositUtxos): SimpleGenesis =
        SimpleGenesis(
          ds.map.values.map(o => SimpleOutput(liftAddress(o.address), o.coins)).toList
        )
    def apply(address: AddressBechL2, ada: Int): SimpleGenesis =
        SimpleGenesis(List(SimpleOutput(address, ada)))

// FIXME: implement
def liftAddress(l: AddressBechL1): AddressBechL2 = AddressBechL2.apply(l.bech32)

case class SimpleTransaction(
    // FIXME: Should be Set, using List for now since Set is not supported in Tapir's Schema deriving
    inputs: List[OutputRefL2],
    outputs: List[SimpleOutput]
)

object SimpleTransaction:
    def apply(input: OutputRefL2, address: AddressBechL2, ada: Int): SimpleTransaction =
        SimpleTransaction(List(input), List(SimpleOutput(address, ada)))

case class SimpleWithdrawal(
    // FIXME: Should be Set, using List for now since Set is not supported in Tapir's Schema deriving
    inputs: List[OutputRefL2]
)

object SimpleWithdrawal:
    def apply(utxo: OutputRefL2): SimpleWithdrawal =
        SimpleWithdrawal(List(utxo))

case class SimpleOutput(
    address: AddressBechL2,
    coins: BigInt
)

type L2Event = AnyL2Event[TxId, SimpleGenesis, SimpleTransaction, SimpleWithdrawal, UtxosDiff]

type L2Genesis = GenesisL2Event[TxId, SimpleGenesis, SimpleTransaction, SimpleWithdrawal, UtxosDiff]

type L2NonGenesis =
    NonGenesisL2Event[TxId, SimpleGenesis, SimpleTransaction, SimpleWithdrawal, UtxosDiff]

type L2Transaction =
    TransactionL2Event[TxId, SimpleGenesis, SimpleTransaction, SimpleWithdrawal, UtxosDiff]

type L2Withdrawal =
    WithdrawalL2Event[TxId, SimpleGenesis, SimpleTransaction, SimpleWithdrawal, UtxosDiff]

type L2EventHash = TxId

object NoopVerifier extends Verifier[Any]:
    def isValid(_event: Any) = true
