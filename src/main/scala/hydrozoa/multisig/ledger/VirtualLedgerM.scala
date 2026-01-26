package hydrozoa.multisig.ledger

import cats.*
import cats.data.*
import cats.effect.IO
import cats.implicits.*
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.multisig.ledger.VirtualLedgerM.Error.{CborParseError, TransactionInvalidError}
import hydrozoa.multisig.ledger.dapp.tx.Tx
import hydrozoa.multisig.ledger.joint.obligation.Payout
import hydrozoa.multisig.ledger.virtual.*
import hydrozoa.multisig.ledger.virtual.commitment.KzgCommitment
import hydrozoa.multisig.ledger.virtual.commitment.KzgCommitment.{KzgCommitment, calculateCommitment, hashToScalar}
import hydrozoa.{emptyContext, given}
import io.bullet.borer.Cbor
import monocle.syntax.all.*
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.{Context, State as ScalusState, UtxoEnv}

private type EV[A] = Either[VirtualLedgerM.Error, A]
private type SV[A] = cats.data.StateT[EV, VirtualLedgerM.State, A]
private type RTV[A] = ReaderT[SV, VirtualLedgerM.Config, A]

/** VirtualLedgerM defines an opaque monad stack for manipulating the [[State]] of the virtual
  * ledger. It's constructor and eliminator methods are private so that it cannot be used in
  * unintended ways.
  *
  * See the companion object for details on allowed operations
  */
case class VirtualLedgerM[A] private (private val unVirtualLedger: RTV[A]) {

    import VirtualLedgerM.*

    private def map[B](f: A => B): VirtualLedgerM[B] = VirtualLedgerM(this.unVirtualLedger.map(f))

    private def flatMap[B](f: A => VirtualLedgerM[B]): VirtualLedgerM[B] =
        VirtualLedgerM(this.unVirtualLedger.flatMap(a => f(a).unVirtualLedger))

    private def run(
        config: VirtualLedgerM.Config,
        initialState: VirtualLedgerM.State
    ): Either[VirtualLedgerM.Error, (VirtualLedgerM.State, A)] =
        this.unVirtualLedger.run(config).run(initialState)

}

object VirtualLedgerM {
    private val ask: VirtualLedgerM[Config] =
        VirtualLedgerM(Kleisli.ask)
    private val get: VirtualLedgerM[VirtualLedgerM.State] =
        VirtualLedgerM(Kleisli.liftF(cats.data.StateT.get))

    /** Applies a genesis event, fully updating the VirtualLedger's state */
    // TODO: Assertion to make sure none of these utxos already exist
    def applyGenesisEvent(genesisEvent: L2EventGenesis): VirtualLedgerM[Unit] =
        for {
            s <- get
            newState = s.copy(s.activeUtxos ++ genesisEvent.asUtxos)
            _ <- set(newState)
        } yield ()

    /** Applies a genesis event to the current virtual ledger state and returns the resulting KZG
      * commitment. We need this because we need to know the "new" KzgCommit prior to settling the
      * ledger, but we don't (maybe?) actually want to apply the genesis event until we know that
      * the settlement tx is valid.
      * @param genesisEvent
      * @return
      */
    // QUESTION: Do we even need this? If we commit and the settlement tx fails and we go to rollback, we don't really
    // care. The rollback will just work autonomously based on whatever KZG commit the treasury utxo has, right?
    //
    // NOTE: there are probably other ways to achieve this -- in particular, I think CPS would allow use to achieve the
    // control flow we need. But I think this is the clearest way to go, and since its cheaper to just compute a hash
    // twice, compared to building a "mock tx" and then adding the real Kzg commit, and more clear than using CPS,
    // I think this is the way to go for now.
    def mockApplyGenesis(genesisEvent: L2EventGenesis): VirtualLedgerM[KzgCommitment] =
        for {
            s <- get
            newState = s.copy(s.activeUtxos ++ genesisEvent.asUtxos)
        } yield newState.kzgCommitment

    def applyInternalTx(
        tx: Tx.Serialized,
        time: QuantizedInstant
    ): VirtualLedgerM[Vector[Payout.Obligation]] =
        given OriginalCborByteArray = OriginalCborByteArray(tx)
        // NOTE: We can probably write a cbor deserialization directly to L2EventTransaction.
        // The question is what conditions we should check during deserialization -- our L2ConformanceValidator
        // is currently run as a ledger validation rule, but could also be run during parsing.
        for {
            l2EventTx <- lift(
              Cbor.decode(tx)
                  .to[Transaction]
                  .valueTry
                  .toEither
                  .bimap(CborParseError.apply, L2EventTransaction.apply)
            )

            s <- get
            config <- ask

            newState <- lift(
              HydrozoaTransactionMutator
                  .transit(
                    time = time,
                    context = config,
                    state = s,
                    l2Event = l2EventTx
                  )
                  .left
                  .map(TransactionInvalidError.apply)
            )

            _ <- set(newState)

            payoutObligations <- lift(
              AddOutputsToUtxoL2Mutator
                  .utxoPartition(l2EventTx)
                  .bimap(
                    TransactionInvalidError.apply,
                    _.payoutObligations
                  )
            )
        } yield payoutObligations

    private def lift[A](e: Either[VirtualLedgerM.Error, A]): VirtualLedgerM[A] =
        VirtualLedgerM(Kleisli.liftF(StateT.liftF(e)))

    private def set(newState: VirtualLedgerM.State): VirtualLedgerM[Unit] =
        VirtualLedgerM(Kleisli.liftF(cats.data.StateT.set(newState)))

    sealed trait Error

    final case class Config(
        cardanoInfo: CardanoInfo
    ) {

        /** Turn into an L1 context with zero fee and an empty CertState
          *
          * @return
          */
        def toL1Context(time: QuantizedInstant, slotConfig: SlotConfig): Context = Context(
          fee = Coin(0),
          env = UtxoEnv(
            time.toSlot.slot,
            cardanoInfo.protocolParams,
            CertState.empty,
            cardanoInfo.network
          ),
          slotConfig = slotConfig
        )
    }

    final case class State(activeUtxos: Map[TransactionInput, TransactionOutput]) {
        lazy val kzgCommitment: KzgCommitment = calculateCommitment(hashToScalar(this.activeUtxos))

        def toScalusState: ScalusState = ScalusState(utxos = activeUtxos)
    }

    object Config:
        val empty: Config = Config.fromL1Context(emptyContext)

        /** Project an L1 context into an L2 context
          */
        private def fromL1Context(l1Context: Context): Config = Config(
          CardanoInfo(
            slotConfig = l1Context.slotConfig,
            protocolParams = l1Context.env.params,
            network = l1Context.env.network
          )
        )

    object State {

        val empty: State = State(activeUtxos = Map.empty)

        def fromScalusState(scalusState: ScalusState): State = State(scalusState.utxos)
    }

    object Error {
        sealed trait ErrorApplyInternalTx extends Error

        sealed trait ErrorApplyGenesisTx extends Error

        final case class CborParseError(e: Throwable) extends Error

        final case class TransactionInvalidError(e: String | TransactionException) extends Error
    }

    extension (jl: JointLedger) {

        /** Run a VirtualLedgerM action within a JointLedger. If the action is successful (returns
          * `Right`), the state of the JointLedger is updated. Because the state update within
          * JointLedger must happen within [[IO]], this takes two continuations (one for success,
          * one for failure) and returns in [[IO]].
          *
          * @param action
          * @param onFailure
          *   continuation if an error is raised. Defaults to throwing an exception.
          * @param onSuccess
          *   continuation if a value is returned. Defaults to IO.pure
          * @tparam A
          * @return
          */
        def runVirtualLedgerM[A, B](
            action: VirtualLedgerM[A],
            onFailure: VirtualLedgerM.Error => IO[B] = e =>
                // FIXME: Type the exception better
                throw new RuntimeException(s"Error running VirtualLedgerM: $e"),
            onSuccess: A => IO[B] = IO.pure[B]
        ): IO[B] = {
            val virtualLedgerMConfig: VirtualLedgerM.Config = VirtualLedgerM.Config(
              jl.config.cardanoInfo
            )
            for {
                oldState <- jl.state.get
                res = action.run(
                  virtualLedgerMConfig,
                  oldState.virtualLedgerState
                )
                b <- res match {
                    case Left(error) => onFailure(error)
                    case Right(newState, a) =>
                        for {
                            _ <- jl.state.set(oldState match {
                                case d: JointLedger.Done =>
                                    d.focus(_.virtualLedgerState).replace(newState)
                                case p: JointLedger.Producing =>
                                    p.focus(_.virtualLedgerState).replace(newState)
                            })
                            b <- onSuccess(a)
                        } yield b
                }
            } yield b
        }
    }
}
