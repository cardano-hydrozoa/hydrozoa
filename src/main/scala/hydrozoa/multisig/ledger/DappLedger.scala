package hydrozoa.multisig.ledger

import cats.effect.{IO, Ref}
import hydrozoa.multisig.ledger.DappLedger.{DepositDecision, ErrorAddDeposit, State, Tx}
import hydrozoa.multisig.ledger.dapp.token.Token.CIP67Tags
import hydrozoa.multisig.ledger.dapp.tx.*
import hydrozoa.multisig.ledger.dapp.utxo.{DepositUtxo, TreasuryUtxo}
import hydrozoa.multisig.ledger.virtual.GenesisObligation
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.AuxiliaryData.Metadata
import scalus.cardano.ledger.{Transaction, TransactionMetadatumLabel, TransactionOutput}

import scala.collection.immutable.Queue
import scala.jdk.CollectionConverters.*
import scala.language.implicitConversions

final case class DappLedger(headAddress: ShelleyAddress)(
    private val state: Ref[IO, State]
) {

    /** Check that a deposit tx is valid and add the deposit utxo it produces to the ledger's state.
      * Return the produced deposit utxo and a post-dated refund transaction for it.
      * @param txSerialized
      *   a serialized deposit transaction
      */
    def registerDeposit(
        txSerialized: Tx.Serialized
    ): IO[Either[ErrorAddDeposit, (DepositUtxo, RefundTx.PostDated)]] = {
        // 1. Deserialize and parse the tx.
        // 2. Check that the deposit tx belongs to this ledger.
        // 3. Check that the tx satisfies ledger STS rules (assuming inputs exist).
        // 4. Append the tx's deposit utxo to the ledger's state.deposits queue.
        // 5. Return the produced deposit utxo and a post-dated refund transaction for it.

        IO.pure(
          // Either Monad
          for {
              tx <- DepositTx.parse(txSerialized)
              refundTx: RefundTx.PostDated = ???
          } yield (tx.depositProduced, refundTx)
        )
    }

    /** Construct a settlement transaction, a fallback transaction, a list of rollout transactions,
      * and a list of immediate refund transactions based on the arguments. Remove the
      * absorbed/refunded deposits and update the treasury in the ledger state.
      * @param depositDecisions
      *   for each deposit, a decision about whether it should be absorbed in the settlement
      *   transaction or immediately refunded.
      * @param payouts
      *   a list of payout outputs that should be produced by the settlement and rollout
      *   transactions.
      *
      * The collective value of the [[payouts]] must '''not''' exceed the [[treasury]] value.
      */
    def settleLedger(
        depositDecisions: List[(DepositUtxo, DepositDecision)],
        payouts: List[TransactionOutput]
    ): IO[
      (
          Option[(SettlementTx, FallbackTx, List[RolloutTx], List[GenesisObligation])],
          List[RefundTx.Immediate]
      )
    ] =
        for {
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
        payouts: List[TransactionOutput]
    ): IO[(FinalizationTx, List[RefundTx.Immediate])] =
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
    def create(
        initRecipe: InitializationTx.Recipe
    ): IO[Either[InitializationTx.BuildError, (DappLedger, InitializationTx)]] = {
        InitializationTx.build(initRecipe) match {
            case Left(e) => IO.pure(Left(e))
            case Right(tx) =>
                for {
                    state <- Ref[IO].of(State(treasury = tx.treasuryProduced))
                } yield Right((DappLedger(headAddress = tx.treasuryProduced.addr)(state), tx))
        }
    }

    final case class State(
        treasury: TreasuryUtxo,
        deposits: Queue[DepositUtxo] = Queue()
    ) {
        // Specialized methods for querying and updating State
    }

    sealed trait DepositDecision
    case class AbsorbDeposit(genesisObligation: GenesisObligation) extends DepositDecision
    case object RefundDeposit extends DepositDecision

    trait Tx {
        val tx: Transaction

        /** A transaction belongs to a [[DappLedger]] if it matches on address and currency symbol
          */
        def txBelongsToLedger(ledger: DappLedger): Boolean =
            this.tx.auxiliaryData.getOrElse(false) match {
                case Metadata(m) =>
                    m.get(TransactionMetadatumLabel(CIP67Tags.head))
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
