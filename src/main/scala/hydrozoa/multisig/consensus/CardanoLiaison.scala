package hydrozoa.multisig.consensus

import cats.effect.{IO, Ref}
import cats.implicits.*
import com.suprnation.actor.Actor.{Actor, Receive}
import hydrozoa.UtxoIdL1
import hydrozoa.multisig.consensus.CardanoLiaison.Config
import hydrozoa.multisig.ledger.dapp.tx.*
import hydrozoa.multisig.ledger.dapp.txseq.{FinalizationTxSeq, RolloutTxSeq, SettlementTxSeq}
import hydrozoa.multisig.protocol.CardanoBackendProtocol.CardanoBackend
import hydrozoa.multisig.protocol.CardanoBackendProtocol.CardanoBackend.{GetCardanoHeadState, GetCardanoHeadStateResp, GetTxInfo, SubmitL1Effects}
import hydrozoa.multisig.protocol.ConsensusProtocol.*
import hydrozoa.multisig.protocol.ConsensusProtocol.CardanoLiaison.*
import hydrozoa.multisig.protocol.types.Block
import scala.collection.immutable.{Seq, TreeMap}
import scala.concurrent.duration.DurationInt
import scalus.cardano.ledger.{Slot, Transaction, TransactionHash, TransactionInput}

/** Hydrozoa's liaison to Cardano L1 (actor):
  *   - Keeps track of the target L1 state the liaison tries to achieve finally.
  *   - Observe all L1-bound block effects (i.e. effects for major and final blocks) and stores them
  *     in the local state.
  *   - Periodically polls the Cardano blockchain for the head's utxo state.
  *   - Submits whichever L1 effects are not yet reflected in the Cardano blockchain.
  *   - Keeps track of confirmed L1 effects of L2 blocks that are immutable on L1 (TODO)
  *
  * Some notes:
  *   - Though it belongs to the multisig regime, the component's lifespan lasts longer since once a
  *     fallback tx gets submitted unfinished rollouts still may exist.
  *   - More broadly, we don't want to die even there is nothing to submit - an L1 rollback may
  *     happen which may require re-applying some effects.
  *   - The core concept the liaison is built around is the "effect", which can be any L1
  *     transaction like initialization, settlement, rollback, deinit or a fallback tx.
  *   - Every effect is tagged with an effect id which is a pair of (block number, index) that
  *     allows running range queries.
  *   - Every effect is associated with a utxo id it can handle.
  *   - L1 utxo state represented by list of utxo ids existent.
  *   - In every run the liaison tries to handle all utxos found with known effects pushing L1
  *     towards the known target state.
  */
object CardanoLiaison {

    object Timeout

    final case class Config(
        cardanoBackend: CardanoBackend.Ref,
        // persistence: Persistence.Ref,
        initializationTx: InitializationTx,
        initializationFallbackTx: FallbackTx
    )

    final case class ConnectionsPending()

    def apply(config: Config): IO[CardanoLiaison] = {
        IO(new CardanoLiaison(config) {})
    }
}

trait CardanoLiaison(config: Config) extends Actor[IO, Request] {
    private val subscribers = Ref.unsafe[IO, Option[Subscribers]](None)
    private val state: Ref[IO, State] = Ref.unsafe[IO, State](State.initialState)

    private final case class Subscribers()

    override def preStart: IO[Unit] =
        for {
            _ <- subscribers.set(Some(Subscribers()))
            // TODO: this is something we likely want to make dynamic
            _ <- context.setReceiveTimeout(5.seconds, CardanoLiaison.Timeout)
        } yield ()

    override def receive: Receive[IO, Request] =
        PartialFunction.fromFunction(req =>
            subscribers.get.flatMap {
                case Some(subs) =>
                    this.receiveTotal(req, subs)
                case _ =>
                    Error(
                      "Impossible: Cardano event actor is receiving before its preStart provided subscribers."
                    ).raiseError
            }
        )

    private def receiveTotal(req: Request, _subs: Subscribers): IO[Unit] =
        req match {
            case effects: ConfirmMajorBlock =>
                handleMajorBlockL1Effects(effects) >> runEffects
            case effects: ConfirmFinalBlock =>
                handleFinalBlockL1Effects(effects) >> runEffects
            case CardanoLiaison.Timeout => runEffects
        }

    // ===================================
    // Inbox handlers
    // ===================================

    /** Handle [[ConfirmMajorBlock]] request:
      *   - saves the effects in the internal actor's state
      */
    // TODO make protected
    def handleMajorBlockL1Effects(block: ConfirmMajorBlock): IO[Unit] = for {
        _ <- IO.println("handleMajorBlockL1Effects")
        // TODO: check effect uniqueness?
        _ <- state.update(s => {
            val (blockEffectInputs, blockEffects) = mkEffectInputsAndEffects(block.settlementTxSeq)
            State(
              targetState = TargetState.Active(
                UtxoIdL1(block.settlementTxSeq.settlementTx.treasuryProduced.utxoId)
              ),
              effectInputs = s.effectInputs ++ blockEffectInputs,
              effects = s.effects ++ blockEffects,
              fallbackTxs = s.fallbackTxs + (block.id -> block.fallbackTx)
            )
        })
    } yield ()

    /** Handle [[ConfirmFinalBlock]] request:
      *   - saves the effects in the internal actor's state
      */
    // TODO make protected
    def handleFinalBlockL1Effects(block: ConfirmFinalBlock): IO[Unit] = for {
        _ <- IO.println("handleFinalBlockL1Effects")
        // TODO: check effect uniqueness?
        _ <- state.update(s => {
            val (blockEffectInputs, blockEffects) =
                mkEffectInputsAndEffects(block.finalizationTxSeq)
            s.copy(
              targetState = TargetState.Finalized(block.finalizationTxSeq.finalizationTx.tx.id),
              effectInputs = s.effectInputs ++ blockEffectInputs,
              effects = s.effects ++ blockEffects
            )
        })
    } yield ()

    private def mkEffectInputsAndEffects(
        settlementTxSeq: SettlementTxSeq
    ): (Seq[(UtxoIdL1, EffectId)], Seq[(EffectId, EffectTx)]) = {
        val settlementTx = settlementTxSeq.settlementTx
        val treasurySpent = settlementTx.treasurySpent
        val blockNumber = Block.Number(settlementTx.majorVersionProduced)
        settlementTxSeq match {
            case _: SettlementTxSeq.NoRollouts =>
                (
                  Seq(UtxoIdL1(treasurySpent.utxoId) -> (blockNumber -> 0)),
                  Seq((blockNumber -> 0) -> EffectSettlementTx(settlementTx))
                )
            case withRollouts: SettlementTxSeq.WithRollouts =>
                val last = withRollouts.rolloutTxSeq.last
                (
                  (treasurySpent.utxoId -> EffectSettlementTx(settlementTx))
                      +: withRollouts.rolloutTxSeq.notLast.map(rolloutTx =>
                          rolloutTx.rolloutSpent.utxo.input -> EffectRolloutTx(rolloutTx)
                      )
                      :+ (last.rolloutSpent.utxo.input -> EffectRolloutTx(last))
                ).zipWithIndex
                    .map((utxoIdAndEffect, index) => {
                        val effectId = blockNumber -> index
                        (UtxoIdL1(
                          utxoIdAndEffect._1
                        ) -> effectId) -> (effectId -> utxoIdAndEffect._2)
                    })
                    .unzip
        }
    }

    private def mkEffectInputsAndEffects(
        finalizationTxSeq: FinalizationTxSeq
    ): (Seq[(UtxoIdL1, EffectId)], Seq[(EffectId, EffectTx)]) =
        val finalizationTx = finalizationTxSeq.finalizationTx
        val treasurySpent = finalizationTx.treasurySpent
        val blockNumber = Block.Number(finalizationTx.majorVersionProduced)
        val finalizationEffectId = blockNumber -> 0
        val finalizationInputEntry = UtxoIdL1(treasurySpent.utxoId) -> finalizationEffectId
        val finalizationEffectEntry = finalizationEffectId -> EffectFinalizationTx(finalizationTx)
        val deinitEffectId = blockNumber.increment -> 0

        def mkWithRollout(
            rolloutTxSeq: RolloutTxSeq
        ): Vector[((UtxoIdL1, (Block.Number, Int)), ((Block.Number, Int), EffectTx))] = {
            val last = rolloutTxSeq.last
            ((treasurySpent.utxoId -> EffectFinalizationTx(finalizationTx))
                +: rolloutTxSeq.notLast.map(rolloutTx =>
                    rolloutTx.rolloutSpent.utxo.input -> EffectRolloutTx(rolloutTx)
                )
                :+ (last.rolloutSpent.utxo.input -> EffectRolloutTx(last))).zipWithIndex
                .map((utxoIdAndEffect, index) => {
                    val effectId = blockNumber -> index
                    (UtxoIdL1(
                      utxoIdAndEffect._1
                    ) -> effectId) -> (effectId -> utxoIdAndEffect._2)
                })
        }

        finalizationTxSeq match {

            case _: FinalizationTxSeq.Monolithic =>
                (
                  Seq(finalizationInputEntry),
                  Seq(finalizationEffectEntry)
                )

            case withDeinit: FinalizationTxSeq.WithDeinit => {
                val deinitInputEffectEntry = UtxoIdL1(
                  withDeinit.deinitTx.residualTreasurySpent.utxoId
                ) -> deinitEffectId
                val deinitEffectEntry = deinitEffectId -> EffectDeinitTx(withDeinit.deinitTx)

                (
                  Seq(
                    finalizationInputEntry,
                    deinitInputEffectEntry
                  ),
                  Seq(
                    finalizationEffectEntry,
                    deinitEffectEntry
                  )
                )
            }

            case withRollouts: FinalizationTxSeq.WithRollouts =>
                mkWithRollout(withRollouts.rolloutTxSeq).unzip

            case withDeinitAndRollout: FinalizationTxSeq.WithDeinitAndRollouts =>
                (mkWithRollout(withDeinitAndRollout.rolloutTxSeq) :+
                    ((UtxoIdL1(
                      withDeinitAndRollout.deinitTx.residualTreasurySpent.utxoId
                    ) -> deinitEffectId) -> (deinitEffectId -> EffectDeinitTx(
                      withDeinitAndRollout.deinitTx
                    )))).unzip
        }

    private def runEffects: IO[Unit] = for {

        _ <- IO.println("runEffects")

        // 1. Get the L1 state, i.e. the list of utxo ids + the current slot
        getCardanoHeadState <- GetCardanoHeadState()
        resp <- config.cardanoBackend ?: getCardanoHeadState

        _ <- resp match {

            case Left(err) =>
                // This may happen if L1 API is temporarily unavailable
                // TODO: we need to address time when we work on autonomous mode
                //   but for now we can just ignore it and skip till the next event/timeout
                IO.println(s"error when getting Cardano L1 state: ${err.msg}")

            case Right(l1State) =>
                for {

                    // 2. Based on the local state, find all due actions
                    state <- state.get

                    // (i.e. those that are directly caused by effect inputs in L1 response).
                    dueActions: Seq[Action] = mkDirectActions(
                      state,
                      l1State.utxoIds,
                      l1State.currentSlot
                    )

                    // 3. Determine whether the initialization requires submission.
                    actionsToSubmit <-
                        if dueActions.nonEmpty
                        then IO.pure(dueActions)
                        else {
                            // Empty direct actions is a precondition for the init (happy path) tx submission
                            lazy val initAction = Seq(
                              Action.InitializeHead(state.effects.values.map(_.tx).toSeq)
                            )
                            // FIXME: check the rule-based treasury!
                            state.targetState match {
                                case TargetState.Active(targetTreasuryUtxoId) =>
                                    IO.pure(
                                      if l1State.utxoIds.contains(targetTreasuryUtxoId)
                                      then List.empty
                                      else initAction
                                    )
                                case TargetState.Finalized(finalizationTxHash) =>
                                    for {
                                        getTxInfo <- GetTxInfo(finalizationTxHash)
                                        txResp <- config.cardanoBackend ?: getTxInfo
                                        mbInitAction <- txResp match {
                                            case Left(err) =>
                                                for {
                                                    _ <- IO.println(
                                                      s"error when getting finalization tx info: ${err.msg}"
                                                    )
                                                } yield Seq.empty
                                            case Right(txInfo) =>
                                                IO.pure(
                                                  if txInfo.isKnown then Seq.empty else initAction
                                                )
                                        }
                                    } yield mbInitAction
                            }
                        }

                    _ <- IO.println(s"actionsToSubmit: ${actionsToSubmit.length}")

                    // 4. Submit flattened txs for actions it there are some
                    _ <- IO.whenA(actionsToSubmit.nonEmpty)(
                      config.cardanoBackend ! SubmitL1Effects(
                        actionsToSubmit.flatMap(actionTxs).toList
                      )
                    )
                } yield ()
        }
    } yield ()

    private def mkDirectActions(state: State, utxosFound: Set[UtxoIdL1], slot: Slot): Seq[Action] =
        // Split into known effect inputs/unknown utxos
        // val (inputs, _unknown) = utxosFound.partition(state.effectInputs.keySet.contains)

        utxosFound
            .map(state.effectInputs.get)
            .filter(_.isDefined)
            .map(_.get)
            .toSeq
            .sorted
            .map(mkDirectAction(state, slot))

    private def mkDirectAction(state: State, currentSlot: Slot)(
        effectId: EffectId
    ): Action.FallbackToRuleBased | Action.PushForwardMultisig | Action.Rollout = {
        effectId match {
            // Backbone tx - settlement/finalization/deinit
            case backboneTx @ (blockNum, 0) =>
                // May absent for phony "deinit" block number
                val mbFallbackTx = state.fallbackTxs.get(blockNum.decrement)
                // By construction either the fallback tx for N is valid or the "settlementOrFinalizationTx" for N+1 is valid:
                // - seems obvious WRT a settlement(finalization)/fallback pair
                // - deinit neither has ttl, nor fallback, so it's always valid
                if mbFallbackTx.isDefined && currentSlot >= mbFallbackTx.get.validityStartSlot then
                    Action.FallbackToRuleBased(mbFallbackTx.get.tx)
                else {
                    val effectTxs =
                        state.effects.rangeFrom(backboneTx).toSeq.map(_._2)
                    Action.PushForwardMultisig(effectTxs.map(_.tx))
                }
            // Rollout tx
            case rolloutTx @ (blockNum, _notZero) =>
                val nextBackboneTx = blockNum.increment -> 0
                val effectTxs = state.effects.range(rolloutTx, nextBackboneTx).toSeq.map(_._2)
                Action.Rollout(effectTxs.map(_.tx))
        }
    }
    // ===================================
    // Internal state
    // ===================================

    /** The second part of the EffectId is a number:
      *   - 0 - settlement
      *   - 1,2,3,... - rollouts
      */
    private type EffectId = (Block.Number, Int)
    private object EffectId:
        val initializationEffectId: EffectId = Block.Number(0) -> 0

    /** The existence of this trait is due to the fact the [[Tx]] trait is not sealed. */
    private sealed trait EffectTx extends Tx:
        def tx: Transaction

    private final case class EffectInitializationTx(dtx: InitializationTx) extends EffectTx {
        override def tx: Transaction = dtx.tx
    }
    private final case class EffectFallbackTx(dtx: FallbackTx) extends EffectTx {
        override def tx: Transaction = dtx.tx
    }
    private final case class EffectSettlementTx(dtx: SettlementTx) extends EffectTx {
        override def tx: Transaction = dtx.tx
    }
    private final case class EffectRolloutTx(dtx: RolloutTx) extends EffectTx {
        override def tx: Transaction = dtx.tx
    }
    private final case class EffectFinalizationTx(dtx: FinalizationTx) extends EffectTx {
        override def tx: Transaction = dtx.tx
    }
    private final case class EffectDeinitTx(dtx: DeinitTx) extends EffectTx {
        override def tx: Transaction = dtx.tx
    }

    /** The state we want to achieve on L1. */
    private enum TargetState:
        /** Regular state of an active head represented by id of the treasury utxo. */
        case Active(treasuryUtxoId: UtxoIdL1)

        /** Final state of a head, represented by the transaction hash of the finalization tx. */
        case Finalized(finalizationTxHash: TransactionHash)

    /** Internal state of the actor. */
    private final case class State(
        /** L1 target state */
        targetState: TargetState,
        /** Contains spent inputs mapping for all effects modulo the initialization tx, since
          * usually it doesn't spend any utxos locked at the head's address, and even if this is the
          * case, the initialization tx is handled separately.
          */
        effectInputs: Map[UtxoIdL1, EffectId],
        /** This contains all effects, the whole fish skeleton, including the initialization tx, but
          * with no fallback txs, which are stored separately in [[fallbackTxs]]
          */
        effects: TreeMap[EffectId, EffectTx],
        /** Fallback txs, indexed by a block number of the preceding settlement tx */
        fallbackTxs: Map[Block.Number, FallbackTx]
    )

    private object State:
        def initialState: State = {
            State(
              targetState =
                  TargetState.Active(UtxoIdL1(config.initializationTx.treasuryProduced.utxoId)),
              effectInputs = Map.empty,
              effects = TreeMap(
                EffectId.initializationEffectId -> EffectInitializationTx(config.initializationTx)
              ),
              fallbackTxs = Map(Block.Number(0) -> config.initializationFallbackTx)
            )
        }

    /** The set of effects the actor may want to execute over L1. */
    private enum Action:
        /** Switching into the rule-based regime. */
        case FallbackToRuleBased(tx: Transaction)

        /** Pushing the existing state in the multisig regime forward. */
        case PushForwardMultisig(txs: Seq[Transaction])

        /** Finalizing rollout sequence. */
        case Rollout(txs: Seq[Transaction])

        /** Like [[PushForwardMultisig]] but starting from the initialization tx. */
        case InitializeHead(txs: Seq[Transaction])

    private def actionTxs(action: Action): Seq[Transaction] = action match {
        case Action.FallbackToRuleBased(tx)  => Seq(tx)
        case Action.PushForwardMultisig(txs) => txs
        case Action.Rollout(txs)             => txs
        case Action.InitializeHead(txs)      => txs
    }

    // ===================================
    // Errors
    // ===================================

    enum Errors extends Throwable:
        /** The state already contains a fallback tx for a particular utxo. */
        case NonUniqueFallbackTx()
}
