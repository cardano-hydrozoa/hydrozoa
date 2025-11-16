package hydrozoa.multisig.ledger

import cats.effect.{IO, Ref}
import hydrozoa.multisig.ledger.DappLedger.{DepositDecision, ErrorAddDeposit, State, Tx}
import hydrozoa.multisig.ledger.dapp.token.CIP67
import hydrozoa.multisig.ledger.dapp.tx.*
import hydrozoa.multisig.ledger.dapp.utxo.{DepositUtxo, TreasuryUtxo}
import hydrozoa.multisig.ledger.virtual.GenesisObligation
import scala.collection.immutable.Queue
import scala.jdk.CollectionConverters.*
import scala.language.implicitConversions
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.AuxiliaryData.Metadata
import scalus.cardano.ledger.{Transaction, TransactionMetadatumLabel, TransactionOutput}
import scalus.cardano.txbuilder.SomeBuildError

// TODO: DappLedger should be an actor. See VirtualLedger
final case class DappLedger(headAddress: ShelleyAddress)(
    private val state: Ref[IO, State]
) {

    /** Check that a deposit tx is valid and add the deposit utxo it produces to the ledger's state.
      * Return the produced deposit utxo and a post-dated refund transaction for it.
     *
      * @param txSerialized
      *   a serialized deposit transaction
      */
    def registerDeposit(
        txSerialized: Tx.Serialized // change to serialized deposit/refund txseq
    ):
    // TODO: The return type should be Either[Error, Unit]. We are checking time bounds
    // here, deposit maturity and stuff.
    IO[Unit] = {
        // 1. Append the tx's deposit utxo + post-dated refund tx 
        // to the ledger's state.deposits queue. This queue should be
      
        IO.pure(???)
    }

    /** Construct a settlement transaction, a fallback transaction, a list of rollout transactions,
      * and a list of immediate refund transactions based on the arguments. Remove the
      * absorbed/refunded deposits and update the treasury in the ledger state.
     * Called when the block weaver sends the single to close the block in leader mode.
    
      * @param deposits
      *   list of deposit utxos that we know currently exist, as passed by the cardano liason
      * @param payouts
      *   a list of payout outputs that should be produced by the settlement and rollout
      *   transactions.
      *
      * The collective value of the [[payouts]] must '''not''' exceed the [[treasury]] value.
     * 
     * @return
     *   Either[Tx Builder Error, or
     *      - SettlementTxSeq (
      */
    def settleLedger(
        deposits: List[(DepositUtxo)], 
        // TODO: make the above Set of event ids (corresponding to deposits). 
        // This is what exists on L1
        // pollResultDeposits : Set[EventId],           
        payouts: List[TransactionOutput],
        // blockCreationTime : PosixTime            
    ): IO[
      (
          // TODO: replace Option with Either to accomodate transaction builder errors
          // TODO: replace with Either[TxBuilderError, (SettlementTxSeq, List[GenesisObligation])]
          Option[(SettlementTx, FallbackTx, List[RolloutTx], List[GenesisObligation])],
          // For the future: we want an immediate refund for deposits that don't exist
          List[RefundTx.Immediate] // TODO: this will be part of fund14 reliability
      )
    ] =
        for {
          //////// Deposits
          // - Keep taking deposits from the queue as long as they are mature
          // - Filter the depositQueue (from the dapp ledger state)
          // according to whether or not the IDs are present in the pollResultsDeposits 
          //   - if it is a member, remove it from poll result set and create a genesis obligation
          //   - if it is not a member, ignore it. TODO: create a refund immediate
          //   - in either case, remove _handled_  deposits from the deposits queue
          //     - We can't remove deposit that were GIVEN to the settlementtxseq builder, but
          //       were not able to be absorbed because tx size issues. these MUST remain in the
          //       queue in the appropriate order.
          //
          // N.B.: JointLedger calls this as a synchronous request. We are not responsible for
          // sending it back to the joint ledger.
            _ <- IO.pure(())
        } yield ???

    /** Construct a finalization transaction, a list of rollout transactions, and a list of
      * immediate refund transactions based on the arguments. The [[DappLedger]] must be discarded
      * after this, so there's no point in updating its state.
      *
      * @param payouts
      *   a list of payout outputs that should be produced by the finalization and rollout
      *   transactions.
      *
      * The collective value of the [[payouts]] must '''not''' exceed the [[treasury]] value.
      * Immediate refund transactions must be constructed for every deposit in the ledger state.
      */
    def finalizeLedger( 
        payouts: List[TransactionOutput] // TODO: List[PayoutObligation]
    ): IO[(FinalizationTx, List[RefundTx.Immediate])] =
       // TODO: IO[Either[TxBuilderError, FinalizationTxSeq]]
       // TODO (fund14): Refund.Immediate 
        for {
            _ <- IO.pure(())
        } yield ???
}

/** ==Hydrozoa's detached dapp ledger on Cardano in the multisig regime==
  *
  * '''Dapp ledger on Cardano''' means that the ledger is domain-specific to a single decentralized
  * application (dApp), and that its state corresponds to a subset of the utxos in the general
  * Cardano ledger. Every state transition of the dapp ledger corresponds to a sequence of one or
  * more Cardano transactions.
  *
  * '''Detached dapp ledger''' means that the ledger's state can be evolved without waiting to
  * synchronize each state transition with Cardano. Instead, the Cardano transactions can be
  * asynchronously submitted to drive Cardano toward the state corresponding to the dapp ledger,
  * repeatedly re-submitting transactions as necessary until they are confirmed on Cardano.
  *
  * Hydrozoa's consensus protocol makes it possible for its dapp ledger to be detached by mitigating
  * the sources of contention that might interfere with the Cardano transactions corresponding to
  * the dapp ledger's transitions.
  */
object DappLedger {

    /** Initialize the L1 ledger's state and return the corresponding initialization transaction. */
    // TODO: replace this with a fully parsed InitializationTxSeq
    // TODO: return type should be Either[Error, DappLedger]. We don't build the InitTx any more
    def create(
        initRecipe: InitializationTx.Recipe
    ): IO[Either[SomeBuildError, (DappLedger, InitializationTx)]] = {
        InitializationTx.build(initRecipe) match {
            case Left(e) => IO.pure(Left(e))
            case Right(tx) =>
                for {
                    state <- Ref[IO].of(State(treasury = tx.treasuryProduced))
                } yield Right((DappLedger(headAddress = tx.treasuryProduced.address)(state), tx))
        }
    }

    final case class State(
        treasury: TreasuryUtxo,
        // TODO: Augment this queue with some additional information to 
        // indicate that we have validated that this transaction has been validated for time                  
        deposits: Queue[DepositUtxo] = Queue()
    ) {
        // Specialized methods for querying and updating State
    }

    // TODO: Remove DepositDecision, because we are the ones deciding now. We are
    // given the existence proof.
    sealed trait DepositDecision
    case class AbsorbDeposit(genesisObligation: GenesisObligation) extends DepositDecision
    case object RefundDeposit extends DepositDecision

    trait Tx {
        def tx: Transaction

        /** A transaction belongs to a [[DappLedger]] if it matches on address and currency symbol
          */
        def txBelongsToLedger(ledger: DappLedger): Boolean =
            this.tx.auxiliaryData.getOrElse(false) match {
                case Metadata(m) =>
                    m.get(TransactionMetadatumLabel(CIP67.Tags.head))
                        .fold(false)(_ == ledger.headAddress)
                case _ => false
            }
    }

    object Tx {
        type Serialized = Array[Byte]
    }

    // We can add some more error types to this ad-hoc union:
    type ErrorAddDeposit = DepositTx.ParseError
}
