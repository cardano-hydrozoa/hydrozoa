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
import scala.concurrent.duration.FiniteDuration
import scalus.cardano.ledger.{Slot, Transaction, TransactionHash, TransactionInput}

/** Hydrozoa's liaison to Cardano L1 (actor):
  *   - Keeps track of the target L1 state the liaison tries to achieve by observing all L1 block
  *     effects (i.e. effects for major and final blocks) and storing them in the local state.
  *   - Periodically polls the Cardano blockchain for the head's utxo state (and some additional
  *     information occasinally).
  *   - Submits whichever L1 effects are not yet reflected in the Cardano blockchain.
  *   - Keeps track of confirmed L1 effects of L2 blocks that are immutable on L1 (TODO F14)
  *
  * Some notes:
  *   - Though this module belongs to the multisig regime, the component's lifespan lasts longer
  *     since once a fallback tx gets submitted unfinished rollouts still may exist.
  *   - More broadly, we don't want to die even there is nothing to submit for a particalar time -
  *     an L1 rollback may happen at any momenet and that may require re-applying some of the
  *     effects.
  *   - The core concept the liaison is built around the "effect", which can be any L1 transaction
  *     like initialization, settlement, rollback, deinit or a fallback tx.
  *   - Every effect is tagged with an _effect id_ which is a pair: (block number, index). This
  *     allows running range queries.
  *   - Every effect is associated with a utxo id it can handle (i.e. spend). This is more efficient
  *     than monitoring which transactions have been already submitted.
  *   - L1 utxo state represented by list of utxo IDs.
  *   - In every run the liaison tries to handle all utxos found with known effects pushing L1
  *     towards the known target state.
  */
object CardanoLiaison {

    object Timeout

    final case class Config(
        cardanoBackend: CardanoBackend.Ref,
        // persistence: Persistence.Ref,
        initializationTx: InitializationTx,
        initializationFallbackTx: FallbackTx,
        receiveTimeout: FiniteDuration
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
            _ <- context.setReceiveTimeout(config.receiveTimeout, CardanoLiaison.Timeout)
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
    // Internal state
    // ===================================

    /** The second part of the EffectId is a number:
      *   - 0 - settlement
      *   - 1,2,3,... - rollouts
      */
    private type EffectId = (Block.Number, Int)

    private object EffectId:
        val initializationEffectId: EffectId = Block.Number(0) -> 0

    /** The state we want to achieve on L1. */
    private enum TargetState:
        /** Regular state of an active head represented by id of the treasury utxo. */
        case Active(treasuryUtxoId: UtxoIdL1)

        /** Final state of a head, represented by the transaction hash of the finalization tx. */
        case Finalized(finalizationTxHash: TransactionHash)

    private type HappyPathEffect = InitializationTx | SettlementTx | FinalizationTx | DeinitTx |
        RolloutTx

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
          * with no fallback txs, which are stored separately in [[fallbackEffects]]
          */
        happyPathEffects: TreeMap[EffectId, HappyPathEffect],

        /** Fallback effects, indexed by the block number they were created. */
        fallbackEffects: Map[Block.Number, FallbackTx]
    )

    private object State:
        def initialState: State = {
            State(
              targetState =
                  TargetState.Active(UtxoIdL1(config.initializationTx.treasuryProduced.utxoId)),
              effectInputs = Map.empty,
              happyPathEffects =
                  TreeMap(EffectId.initializationEffectId -> config.initializationTx),
              fallbackEffects = Map(Block.Number(0) -> config.initializationFallbackTx)
            )
        }

    // ===================================
    // Inbox handlers
    // ===================================

    /** Handle [[ConfirmMajorBlock]] request:
      *   - saves the effects in the internal actor's state
      */
    protected[consensus] def handleMajorBlockL1Effects(block: ConfirmMajorBlock): IO[Unit] = for {
        _ <- IO.println("handleMajorBlockL1Effects")
        // TODO: check effect uniqueness?
        _ <- state.update(s => {
            val (blockEffectInputs, blockEffects) =
                mkHappyPathEffectInputsAndEffects(block.settlementTxSeq)
            State(
              targetState = TargetState.Active(
                UtxoIdL1(block.settlementTxSeq.settlementTx.treasuryProduced.utxoId)
              ),
              effectInputs = s.effectInputs ++ blockEffectInputs,
              happyPathEffects = s.happyPathEffects ++ blockEffects,
              fallbackEffects = s.fallbackEffects + (block.id -> block.fallbackTx)
            )
        })
    } yield ()

    /** Handle [[ConfirmFinalBlock]] request:
      *   - saves the effects in the internal actor's state
      */
    protected[consensus] def handleFinalBlockL1Effects(block: ConfirmFinalBlock): IO[Unit] = for {
        _ <- IO.println("handleFinalBlockL1Effects")
        // TODO: check effect uniqueness?
        _ <- state.update(s => {
            val (blockEffectInputs, blockEffects) =
                mkHappyPathEffectInputsAndEffects(block.finalizationTxSeq)
            s.copy(
              targetState = TargetState.Finalized(block.finalizationTxSeq.finalizationTx.tx.id),
              effectInputs = s.effectInputs ++ blockEffectInputs,
              happyPathEffects = s.happyPathEffects ++ blockEffects
            )
        })
    } yield ()

    private def mkHappyPathEffectInputsAndEffects(
        settlementTxSeq: SettlementTxSeq
    ): (
        Seq[(UtxoIdL1, EffectId)],
        Seq[(EffectId, HappyPathEffect)]
    ) = {
        val settlementTx = settlementTxSeq.settlementTx
        val treasurySpent = settlementTx.treasurySpent
        val blockNumber = Block.Number(settlementTx.majorVersionProduced)
        settlementTxSeq match {
            case _: SettlementTxSeq.NoRollouts =>
                (
                  Seq(UtxoIdL1(treasurySpent.utxoId) -> (blockNumber -> 0)),
                  Seq((blockNumber -> 0) -> settlementTx)
                )
            case withRollouts: SettlementTxSeq.WithRollouts =>
                val last = withRollouts.rolloutTxSeq.last
                val vector: Vector[(TransactionInput, HappyPathEffect)] = (
                  (treasurySpent.utxoId -> settlementTx)
                      +: withRollouts.rolloutTxSeq.notLast.map(rolloutTx =>
                          rolloutTx.rolloutSpent.utxo.input -> rolloutTx
                      )
                      :+ (last.rolloutSpent.utxo.input -> last)
                )
                vector.zipWithIndex
                    .map((utxoIdAndEffect, index) => {
                        val effectId = blockNumber -> index
                        (UtxoIdL1(
                          utxoIdAndEffect._1
                        ) -> effectId) -> (effectId -> utxoIdAndEffect._2)
                    })
                    .unzip
        }
    }

    private def mkHappyPathEffectInputsAndEffects(
        finalizationTxSeq: FinalizationTxSeq
    ): (
        Seq[(UtxoIdL1, EffectId)],
        Seq[(EffectId, HappyPathEffect)]
    ) =
        val finalizationTx = finalizationTxSeq.finalizationTx
        val treasurySpent = finalizationTx.treasurySpent
        val blockNumber = Block.Number(finalizationTx.majorVersionProduced)
        val finalizationEffectId = blockNumber -> 0
        val finalizationInputEntry = UtxoIdL1(treasurySpent.utxoId) -> finalizationEffectId
        val finalizationEffectEntry = finalizationEffectId -> finalizationTx
        val deinitEffectId = blockNumber.increment -> 0

        def mkWithRollout(
            rolloutTxSeq: RolloutTxSeq
        ): Vector[((UtxoIdL1, (Block.Number, Int)), ((Block.Number, Int), HappyPathEffect))] = {
            val last = rolloutTxSeq.last
            ((treasurySpent.utxoId -> finalizationTx)
                +: rolloutTxSeq.notLast.map(rolloutTx =>
                    rolloutTx.rolloutSpent.utxo.input -> rolloutTx
                )
                :+ (last.rolloutSpent.utxo.input -> last)).zipWithIndex
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
                val deinitEffectEntry = deinitEffectId -> withDeinit.deinitTx

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
                    ) -> deinitEffectId) -> (deinitEffectId -> withDeinitAndRollout.deinitTx))).unzip
        }

    /** The core part of the liaison that decides whether an action is needed and submits them.
      *
      * It's called either when:
      *   - the liaison learns a new effect
      *   - by receiving timeout
      */
    private def runEffects: IO[Unit] = for {

        _ <- IO.println("runEffects")

        // 1. Get the L1 state, i.e. the list of utxo ids at the multisig address  + the current slot
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
                    ).fold(e => throw RuntimeException(e.msg), x => x)

                    // 3. Determine whether the initialization requires submission.
                    actionsToSubmit <-
                        if dueActions.nonEmpty
                        then IO.pure(dueActions)
                        else {
                            // Empty direct actions is a precondition for the init (happy path) tx submission
                            lazy val initAction = Seq(
                              Action.InitializeHead(state.happyPathEffects.values.map(_.tx).toSeq)
                            )
                            // TODO: check the rule-based treasury, and if it exists, don't try to initialize the head.
                            state.targetState match {
                                case TargetState.Active(targetTreasuryUtxoId) =>
                                    IO.pure(
                                      if l1State.utxoIds.contains(targetTreasuryUtxoId)
                                      then List.empty // everything is up-to-date on L1
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

    // ===================================
    // Actions
    // ===================================

    /** The set of effects the actor may want to execute against L1. */
    private enum Action:
        /** Switching into the rule-based regime. */
        case FallbackToRuleBased(tx: Transaction)

        /** Pushing the existing state in the multisig regime forward. */
        case PushForwardMultisig(txs: Seq[Transaction])

        /** Finalizing a rollout sequence. */
        case Rollout(txs: Seq[Transaction])

        /** Represents noop action that may occur when the current slot falls into the silence
          * period - a gap between two competeing transactions when the settlement/finzalization tx
          * already expired but the fallback is not valid yet.
          */
        case SilencePeriodNoop

        /** Like [[PushForwardMultisig]] but starting from the initialization tx. */
        case InitializeHead(txs: Seq[Transaction])

    private def actionTxs(action: Action): Seq[Transaction] = action match {
        case Action.FallbackToRuleBased(tx)  => Seq(tx)
        case Action.PushForwardMultisig(txs) => txs
        case Action.Rollout(txs)             => txs
        case Action.SilencePeriodNoop        => Seq.empty
        case Action.InitializeHead(txs)      => txs
    }

    private type DirectAction =
        Action.FallbackToRuleBased | Action.PushForwardMultisig | Action.Rollout |
            Action.SilencePeriodNoop.type

    private def mkDirectActions(
        state: State,
        utxosFound: Set[UtxoIdL1],
        slot: Slot
    ): Either[EffectError, Seq[DirectAction]] =
        utxosFound
            .map(state.effectInputs.get)
            .filter(_.isDefined)
            .map(_.get)
            .toSeq
            .sorted
            .map(mkDirectAction(state, slot))
            .sequence

    private def mkDirectAction(state: State, currentSlot: Slot)(
        effectId: EffectId
    ): Either[EffectError, DirectAction] =
        import Action.*
        import EffectError.*

        effectId match {
            // Backbone effect - settlement/finalization/deinit
            case backboneEffectId @ (blockNum, 0) =>

                val happyPathEffect = state.happyPathEffects(backboneEffectId)
                // May absent for phony "deinit" block number
                val mbCompetingFallbackEffect = state.fallbackEffects.get(blockNum.decrement)

                // Invariant: there should be always one sensible outcome:
                // - (1) either the settlement/finalization/deinit tx for block N+1 is valid
                // - (2) or we are inside the silence period
                // - (2) or the fallback tx for N is valid

                // This is obvious for a regular settlement(finalization)/fallback pair, and for
                // the deinit, that has neither has ttl, nor a competing fallback, (1) is always true.
                mbCompetingFallbackEffect match {

                    // Not a deinit effect
                    case Some(fallback) =>
                        for {
                            happyPathTxTtl <- happyPathEffect match {
                                case tx: InitializationTx => Right(tx.ttl)
                                case tx: SettlementTx     => Right(tx.ttl)
                                case tx: FinalizationTx   => Right(tx.ttl)
                                // TODO: this should never happen
                                case _ => Left(UpexpectedEffectType)
                            }

                            fallbackValidityStart = fallback.validityStart

                            // Choose between (1), (2), and (3)
                            ret <- (
                              currentSlot,
                              happyPathTxTtl,
                              fallbackValidityStart
                            ) match {
                                // (1)
                                case _ if currentSlot < happyPathTxTtl =>
                                    val effectTxs =
                                        state.happyPathEffects
                                            .rangeFrom(backboneEffectId)
                                            .toSeq
                                            .map(_._2)
                                    Right(PushForwardMultisig(effectTxs.map(_.tx)): DirectAction)
                                // (2)
                                case _
                                    if currentSlot >= happyPathTxTtl && currentSlot < fallbackValidityStart =>
                                    Right(SilencePeriodNoop: DirectAction)
                                // (3)
                                case _ if currentSlot >= fallbackValidityStart =>
                                    Right(
                                      FallbackToRuleBased(
                                        mbCompetingFallbackEffect.get.tx
                                      ): DirectAction
                                    )
                                // Should never happen, indicates an error in validity range calculation
                                case _ =>
                                    Left(
                                      WrongValidityRange(
                                        currentSlot,
                                        happyPathTxTtl,
                                        fallbackValidityStart
                                      )
                                    )
                            }
                            // TODO: I wasn't able to remove that "safe" cast
                        } yield ret.asInstanceOf[DirectAction]

                    // This is a deinit effect, always the last tx in the backbone
                    case None =>
                        val deinitTxEffectId = backboneEffectId
                        Right(PushForwardMultisig(Seq(state.happyPathEffects(deinitTxEffectId).tx)))
                }

            // Rollout tx
            case rolloutTx @ (blockNum, _notZero) =>
                val nextBackboneTx = blockNum.increment -> 0
                val effectTxs =
                    state.happyPathEffects.range(rolloutTx, nextBackboneTx).toSeq.map(_._2)
                Right(Rollout(effectTxs.map(_.tx)))
        }

    end mkDirectAction

    private enum EffectError extends Throwable:
        case UpexpectedEffectType
        case WrongValidityRange(currentSlot: Slot, happyPathTtl: Slot, fallbackValidityStart: Slot)

    extension (self: EffectError)
        private def msg: String = self match {
            case EffectError.UpexpectedEffectType =>
                "Unexpected effect type was encountered, check the integrity of effects."
            case EffectError.WrongValidityRange(currentSlot, happyPathTtl, fallbackValidityStart) =>
                s"Validity range invariant is not hold: current slot: ${currentSlot}," +
                    s" happy path tx TTL: ${happyPathTtl} fallback validity start: ${fallbackValidityStart}"
        }
}
