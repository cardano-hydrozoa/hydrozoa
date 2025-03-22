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

// TODO: Add phantom type to reflect the purpose.
type UtxosSet = Set[(UtxoIdL2, Output[L2])]

// FIXME: move InstancePurpose to L2Ledger
// FIXME: don't combine case class and "state"-class
case class AdaSimpleLedger[InstancePurpose <: TInstancePurpose] private (
    verifier: Verifier[EventL2]
) extends L2Ledger[
      UtxosSetOpaque,
      UtxosSet,
      EventHash,
      SimpleGenesis,
      SimpleTransaction,
      SimpleWithdrawal,
      EventL2
    ]:

    private val activeState: UtxosSetOpaqueMutable = mutable.Map.empty

    override def getUtxosActive: UtxosSetOpaque = activeState.clone.toMap

    override def replaceUtxosActive(activeState: UtxosSetOpaque): Unit =
        // TODO: revise
        this.activeState.clear()
        this.activeState.addAll(activeState)

    /** Makes a copy of the current ledger for block production purposes.
      * @param ev
      *   evidence that we are cloning Hydrozoa ledger, not its clone
      * @return
      *   cloned ledger
      */
    def blockProduction(using
        ev: InstancePurpose =:= THydrozoaHead
    ): AdaSimpleLedger[TBlockProduction] =
        val ledgerForBlockProduction: AdaSimpleLedger[TBlockProduction] = copy()
        ledgerForBlockProduction.replaceUtxosActive(activeState.clone().toMap)
        ledgerForBlockProduction

    override def submit[E1 <: EventL2](
        event: E1
    ): Either[(EventHash, String), (EventHash, event.UtxosDiff)] =
        require(verifier.isValid(event), true)
        event match
            case genesis: GenesisL2       => handleGenesis(genesis)
            case tx: TransactionL2        => handleTransaction(tx)
            case withdrawal: WithdrawalL2 => handleWithdrawal(withdrawal)

    private def handleGenesis(event: GenesisL2) =
        val (_, txId) = AdaSimpleLedger.asTxL2(event.genesis)

        val utxoDiff = event.genesis.outputs.zipWithIndex
            .map(output =>
                val txIn = UtxoIdL2(txId, TxIx(output._2))
                val txOut = Output[L2](output._1.address.asL1, output._1.coins)
                (txIn, txOut)
            )
            .toSet

        val utxoDiffInt = event.genesis.outputs.zipWithIndex
            .map(output =>
                val txIn = liftOutputRef(UtxoIdL2(txId, TxIx(output._2)))
                val txOut = liftOutput(output._1.address, output._1.coins)
                (txIn, txOut)
            )
            .toSet

        activeState.addAll(utxoDiffInt)
        Right((txId, utxoDiff))

    private def handleTransaction(event: TransactionL2) =
        val (_, txId) = AdaSimpleLedger.asTxL2(event.transaction)

        resolveInputs(event.transaction.inputs) match
            case Left(extraneous) =>
                Left(txId, s"Extraneous utxos in transaction $txId: $extraneous")
            case Right(oldUtxos) =>
                // Outputs
                val newUtxos = event.transaction.outputs.zipWithIndex.map(output =>
                    val txIn = liftOutputRef(UtxoIdL2(txId, TxIx(output._2)))
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

                    Right((txId, Set[(UtxoIdL2, Output[L2])]()))

    private def handleWithdrawal(event: WithdrawalL2) =
        val (_, txId) = AdaSimpleLedger.asTxL2(event.withdrawal)

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
        inputs: List[UtxoIdL2]
    ): Either[List[UtxoIdL2], List[(OutputRefInt, OutputInt, (UtxoIdL2, Output[L2]))]] =
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

    override def flush: UtxosSet =
        val ret = activeState.clone()
        activeState.clear()
        ret.toSet.map((k, v) => (unliftOutputRef(k), unliftOutput(v)))

object AdaSimpleLedger:
    def apply(): AdaSimpleLedger[THydrozoaHead] = AdaSimpleLedger[THydrozoaHead](NoopVerifier)

    def asTxL2(event: SimpleGenesis | SimpleTransaction | SimpleWithdrawal): (TxL2, EventHash) =
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

    def mkGenesisEvent(genesis: SimpleGenesis): GenesisL2 =
        val (_, txId) = asTxL2(genesis)
        GenesisEventL2(txId, genesis)

    def mkTransactionEvent(tx: SimpleTransaction): TransactionL2 =
        val (_, txId) = asTxL2(tx)
        TransactionEventL2(txId, tx)

    def mkWithdrawalEvent(withdrawal: SimpleWithdrawal): WithdrawalL2 =
        val (_, txId) = asTxL2(withdrawal)
        WithdrawalEventL2(txId, withdrawal)

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
    inputs: List[UtxoIdL2],
    outputs: List[SimpleOutput]
)

object SimpleTransaction:
    def apply(input: UtxoIdL2, address: AddressBechL2, ada: Int): SimpleTransaction =
        SimpleTransaction(List(input), List(SimpleOutput(address, ada)))

case class SimpleWithdrawal(
    // FIXME: Should be Set, using List for now since Set is not supported in Tapir's Schema deriving
    inputs: List[UtxoIdL2]
)

object SimpleWithdrawal:
    def apply(utxo: UtxoIdL2): SimpleWithdrawal =
        SimpleWithdrawal(List(utxo))

case class SimpleOutput(
    address: AddressBechL2,
    coins: BigInt
)

type EventL2 = AnyEventL2[TxId, SimpleGenesis, SimpleTransaction, SimpleWithdrawal, UtxosSet]

type GenesisL2 = GenesisEventL2[TxId, SimpleGenesis, SimpleTransaction, SimpleWithdrawal, UtxosSet]

type NonGenesisL2 =
    NonGenesisEventL2[TxId, SimpleGenesis, SimpleTransaction, SimpleWithdrawal, UtxosSet]

type TransactionL2 =
    TransactionEventL2[TxId, SimpleGenesis, SimpleTransaction, SimpleWithdrawal, UtxosSet]

type WithdrawalL2 =
    WithdrawalEventL2[TxId, SimpleGenesis, SimpleTransaction, SimpleWithdrawal, UtxosSet]

type EventHash = TxId

object NoopVerifier extends Verifier[Any]:
    def isValid(_event: Any) = true
