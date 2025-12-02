package hydrozoa.multisig.ledger

import cats.effect.*
import cats.implicits.catsSyntaxFlatMapOps
import com.suprnation.actor.Actor.{Actor, Receive}
import hydrozoa.given
import hydrozoa.lib.actor.SyncRequest
import hydrozoa.multisig.ledger.VirtualLedger.*
import hydrozoa.multisig.ledger.virtual.*
import hydrozoa.multisig.ledger.virtual.commitment.KzgCommitment
import hydrozoa.multisig.ledger.virtual.commitment.KzgCommitment.KzgCommitment
import hydrozoa.{emptyContext, emptyState}
import io.bullet.borer.Cbor
import scala.util.{Failure, Success}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.State as ScalusState

private def toScalusState(state: State): ScalusState =
    emptyState.copy(utxos = state.activeUtxos)

private def fromScalusState(sstate: ScalusState): State =
    State(sstate.utxos)

// TODO: We want to collapse "InternalTx" and "WithdrawalTx" into a single tx type
// where the L1-bound and L2-bound utxos are distinguihed in the metadata
trait VirtualLedger(config: Config) extends Actor[IO, Request] {
    private val state: Ref[IO, State] = Ref.unsafe[IO, State](State(Map.empty))

    override def receive: Receive[IO, Request] = PartialFunction.fromFunction {
        case ApplyInternalTx(tx, d)   => applyInternalTx(tx) >>= (res => d.complete(res))
        case ApplyWithdrawalTx(tx, d) => applyWithdrawalTx(tx) >>= (res => d.complete(res))
        case ApplyGenesis(go)         => applyGenesisTx(go)
    }

    private def applyInternalTx(
        txSerialized: Tx.Serialized
    ): IO[Either[ErrorApplyInternalTx, Unit]] =
        given OriginalCborByteArray = OriginalCborByteArray(txSerialized)
        // NOTE: We can probably write a cbor deserialization directly to L2EventTransaction.
        // The question is what conditions we should check during deserialization -- our L2ConformanceValidator
        // is currently run as a ledger validation rule, but could also be run during parsing.
        Cbor.decode(txSerialized).to[Transaction].valueTry match {
            case Failure(e) => IO.pure(Left(CborParseError(e)))
            case Success(tx) =>
                for {
                    s <- state.get
                    scalusState = toScalusState(s)
                    res: Either[ErrorApplyInternalTx, Unit] <- HydrozoaTransactionMutator(
                      context = emptyContext,
                      state = scalusState,
                      event = L2EventTransaction(tx)
                    ) match {
                        case Left(e)  => IO.pure(Left(TransactionInvalidError(e)))
                        case Right(v) => state.set(fromScalusState(v)).map(Right(_))
                    }

                } yield res
        }

    private def applyWithdrawalTx(
        txSerialized: Tx.Serialized
    ): IO[Either[ErrorApplyWithdrawalTx, List[TransactionOutput]]] =
        given OriginalCborByteArray = OriginalCborByteArray(txSerialized)
        Cbor.decode(txSerialized).to[Transaction].valueTry match {
            case Failure(e) => IO.pure(Left(CborParseError(e)))
            case Success(tx) =>
                for {
                    s <- state.get
                    scalusState = toScalusState(s)
                    res: Either[ErrorApplyInternalTx, Unit] <- HydrozoaWithdrawalMutator(
                      context = emptyContext,
                      state = scalusState,
                      event = L2EventTransaction(tx)
                    ) match {
                        case Left(e)  => IO.pure(Left(TransactionInvalidError(e)))
                        case Right(v) => state.set(fromScalusState(v)).map(Right(_))
                    }

                } yield Right(
                  tx.body.value.inputs.toSeq.foldLeft(List.empty)((acc, ti) =>
                      // N.B.: s.activeUtxos(ti) is technically partial, but this SHOULD be caught
                      // at the hydrozoa withdrawal mutator. If this branch is run, it should
                      // mean that all the transaction inputs existed in the active utxo set.
                      acc.appended(s.activeUtxos(ti))
                  )
                )
        }

    // TODO: We apply genesis obligations. These aren't "transactions"; this function
    // needs a name change.
    private def applyGenesisTx(tx: L2EventGenesis): IO[Unit] =
        state.get >>= (s => state.set(HydrozoaGenesisMutator.addGenesisUtxosToState(g = tx._1, s)))

    def makeUtxosCommitment: IO[KzgCommitment] =
        for {
            s <- state.get
            utxoHashed <- IO(KzgCommitment.hashToScalar(s.activeUtxos))
            commitment <- IO(KzgCommitment.calculateCommitment(utxoHashed))
        } yield commitment
}

/** ==Hydrozoa's open virtual ledger in the multisig regime==
  *
  * We define an '''open ledger''' to be a ledger that tracks the data modified by a system, S, such
  * that the transitions (think "transactions", but possibly not exclusively Cardano L1
  * transactions) within the system consume data that exists at a "boundary". The boundary is,
  * informally, a "port" or "interface" where another system can "place" data (thus, by default,
  * untrusted) or "consume" data (thus, by default, the boundary is volatile in the sense that it
  * can be exogenously modified).
  *
  * A '''closed ledger''' is a ledger that _cannot_ consume data at a boundary. Note that a close
  * ledger can still _produce_ data at a boundary, but it is "write only". The data that ends up at
  * this boundary element therefore cannot affect the semantics of the rest of the ledger.
  *
  * We define a '''virtual ledger''' as a (sub-)ledger that exists entirely internal to a system, S,
  * in the following way:
  *
  *   - The data tracked by the virtual ledger can only be modified by a set of "virtual transaction
  *     families", F
  *   - Each transaction T \in F_i consumes data (in our case, utxos/token mints) that can _only_ be
  *     produced from transaction families in S, and does not consume data that is produced by
  *     transactions belonging to families not in S
  *
  * In other words, a virtual ledger is a closed sub-ledger of a possibly-open ledger; it is a
  * ledger that only tracks data related to transactions that do not come from the boundaries of the
  * system. Every closed ledger is a virtual ledger; an open ledger may or may not contain one or
  * more virtual subledgers.
  *
  * If a subledger L of system S is virtual, it means that the virtual ledger's state can transition
  * without necessarily corresponding to any transitions in another system, O; the system S is in
  * _full control_ of the transitions of the transitions of L, and doesn't have to "ask" or
  * "reconcile" with O.
  *
  * Hydrozoa's joint ledger, H, is an open ledger. Boundaries exist for funding deposits (the
  * funding UTxOs come from "outside" of hydrozoa), producing payout and rollout UTxOs, and various
  * signals that must arise from L1 or L2 consensus.
  *
  * The multisig regime has a virtual subledger comprising the L2 transaction families of
  * "L2Genesis", "L2Withdrawal", and "L2Transaction". Under the assumption of multisig consensus,
  * these transaction families are entirely internal to ledger H, and therefore can be advanced
  * without worrying about the volatile state at the boundary.
  *
  * (Note that being "virtual" is a _property_ of the design of the L2 subledger of H, and not given
  * a priori by the fact that it is an L2. Similarly, the fact that no L1 transaction families are
  * included in the virtual ledger is not a priori given by being L1; it is a consequence of the
  * design. Virtuality and L1/L2 are independent properties in the general case.)
  */
object VirtualLedger {
    def apply(config: Config): IO[VirtualLedger] =
        IO(new VirtualLedger(config) {})

    ///////////////////////////////////////////
    // Requests
    type Request = ApplyInternalTx | ApplyWithdrawalTx | ApplyGenesis

    // Internal Tx
    final case class ApplyInternalTx(
        tx: Tx.Serialized,
        override val dResponse: Deferred[IO, Either[ErrorApplyInternalTx, Unit]]
    ) extends SyncRequest[IO, ErrorApplyInternalTx, Unit]

    object ApplyInternalTx {

        def apply(tx: Tx.Serialized): IO[ApplyInternalTx] = for {
            deferredResponse <- Deferred[IO, Either[ErrorApplyInternalTx, Unit]]
        } yield ApplyInternalTx(tx, deferredResponse)
    }

    // Withdrawal Tx
    final case class ApplyWithdrawalTx(
        tx: Tx.Serialized,
        override val dResponse: Deferred[
          IO,
          Either[ErrorApplyWithdrawalTx, List[TransactionOutput]]
        ]
    ) extends SyncRequest[IO, ErrorApplyWithdrawalTx, List[TransactionOutput]]

    object ApplyWithdrawalTx {

        def apply(tx: Tx.Serialized): IO[ApplyWithdrawalTx] = for {
            deferredResponse <- Deferred[IO, Either[ErrorApplyWithdrawalTx, List[
              TransactionOutput
            ]]]
        } yield ApplyWithdrawalTx(tx, deferredResponse)
    }

    // Genesis Tx
    final case class ApplyGenesis(go: L2EventGenesis)

    //////////////////////////////////////////////
    // Other Types
    final case class Config(
        protocolParams: Unit
    )

    final case class State(
        activeUtxos: Map[TransactionInput, TransactionOutput]
    )

    sealed trait Tx {
        val tx: Transaction
    }

    object Tx {
        type Serialized = Array[Byte]
    }

    sealed trait ErrorApplyInternalTx extends Throwable

    sealed trait ErrorApplyWithdrawalTx extends Throwable

    sealed trait ErrorApplyGenesisTx extends Throwable

    final case class CborParseError(e: Throwable)
        extends Throwable,
          ErrorApplyInternalTx,
          ErrorApplyWithdrawalTx,
          ErrorApplyGenesisTx
    final case class TransactionInvalidError(e: String | TransactionException)
        extends Throwable,
          ErrorApplyInternalTx,
          ErrorApplyWithdrawalTx,
          ErrorApplyGenesisTx
}
