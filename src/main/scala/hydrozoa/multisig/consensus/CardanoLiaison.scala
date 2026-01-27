package hydrozoa.multisig.consensus

import cats.effect.{IO, Ref}
import cats.implicits.*
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.ActorRef
import hydrozoa.UtxoIdL1
import hydrozoa.lib.cardano.scalus.QuantizedTime.{QuantizedInstant, toEpochQuantizedInstant}
import hydrozoa.multisig.MultisigRegimeManager
import hydrozoa.multisig.backend.cardano.CardanoBackend
import hydrozoa.multisig.consensus.BlockWeaver.PollResults
import hydrozoa.multisig.ledger.block.{BlockEffects, BlockHeader, BlockNumber}
import hydrozoa.multisig.ledger.dapp.tx.*
import scala.collection.immutable.{Seq, TreeMap}
import scala.concurrent.duration.FiniteDuration
import scala.math.Ordered.orderingToOrdered
import scalus.cardano.ledger.{Block as _, BlockHeader as _, SlotConfig, Transaction, TransactionHash, TransactionInput}

/** Hydrozoa's liaison to Cardano L1 (actor):
  *   - Keeps track of the target L1 state the liaison tries to achieve by observing all L1 block
  *     effects (i.e. effects for major and final blocks) and storing them in the local state.
  *   - Periodically polls the Cardano blockchain for the head's utxo state (and some additional
  *     information occasionally).
  *   - Submits whichever L1 effects are not yet reflected in the Cardano blockchain.
  *   - Keeps track of confirmed L1 effects of L2 blocks that are immutable on L1 (TODO F14)
  *
  * Some notes:
  *   - Though this module belongs to the multisig regime, the component's lifespan lasts longer
  *     since once a fallback tx gets submitted unfinished rollouts still may exist.
  *   - More broadly, we don't want to die even there is nothing to submit for a particular time -
  *     an L1 rollback may happen at any moment and that may require re-applying some of the
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
object CardanoLiaison:
    def apply(
        config: Config,
        pendingConnections: MultisigRegimeManager.PendingConnections | CardanoLiaison.Connections
    ): IO[CardanoLiaison] =
        IO(new CardanoLiaison(config, pendingConnections) {})

    final case class Config(
        cardanoBackend: CardanoBackend[IO],
        initializationTx: InitializationTx,
        initializationFallbackTx: FallbackTx,
        receiveTimeout: FiniteDuration,
        slotConfig: SlotConfig
    )

    final case class Connections(
        blockWeaver: BlockWeaver.Handle
    )

    // ===================================
    // Actor's Internal state
    // ===================================

    /** The second part of the EffectId is a number:
      *   - 0 - settlement
      *   - 1,2,3,... - rollouts
      */
    type EffectId = (BlockNumber, Int)

    object EffectId:
        val initializationEffectId: EffectId = BlockNumber(0) -> 0

    /** The state we want to achieve on L1. */
    enum TargetState:
        /** Regular state of an active head represented by id of the treasury utxo. */
        case Active(treasuryUtxoId: UtxoIdL1)

        /** Final state of a head, represented by the transaction hash of the finalization tx. */
        case Finalized(finalizationTxHash: TransactionHash)

    type HappyPathEffect = InitializationTx | SettlementTx | FinalizationTx | DeinitTx | RolloutTx

    extension (effect: HappyPathEffect)
        def tx: Transaction = effect match
            case e: InitializationTx => e.tx
            case e: SettlementTx     => e.tx
            case e: FinalizationTx   => e.tx
            case e: DeinitTx         => e.tx
            case e: RolloutTx        => e.tx

    /** Internal state of the actor. */
    final case class State(
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
        fallbackEffects: Map[BlockNumber, FallbackTx]
    )

    object State:
        def initialState(config: Config): State = {
            State(
              targetState =
                  TargetState.Active(UtxoIdL1(config.initializationTx.treasuryProduced.utxoId)),
              effectInputs = Map.empty,
              happyPathEffects =
                  TreeMap(EffectId.initializationEffectId -> config.initializationTx),
              fallbackEffects = Map(BlockNumber(0) -> config.initializationFallbackTx)
            )
        }

    // ===================================
    // Request + ActorRef + apply
    // ===================================
    object Timeout

    object BlockConfirmed {
        type Major = BlockHeader.Fields.HasBlockNum & BlockEffects.MultiSigned.Major.Section
        type Final = BlockHeader.Fields.HasBlockNum & BlockEffects.MultiSigned.Final.Section

        /** For testing purposes, where we may not want to construct a whole Block.MultiSigned. */
        sealed trait Minimal extends BlockHeader.Fields.HasBlockNum

        object Minimal {

            /** For testing purposes, where we may not want to construct a whole Block.MultiSigned.
              */
            final case class Major(
                override val blockNum: BlockNumber,
                override val settlementTx: SettlementTx,
                override val fallbackTx: FallbackTx,
                override val rolloutTxs: List[RolloutTx],
                override val postDatedRefundTxs: List[RefundTx.PostDated],
            ) extends Minimal,
                  BlockEffects.MultiSigned.Major.Section {
                override def effects: BlockEffects.MultiSigned.Major = BlockEffects.MultiSigned
                    .Major(settlementTx, rolloutTxs, fallbackTx, postDatedRefundTxs)
            }

            /** For testing purposes, where we may not want to construct a whole Block.MultiSigned.
              */
            final case class Final(
                override val blockNum: BlockNumber,
                override val finalizationTx: FinalizationTx,
                override val rolloutTxs: List[RolloutTx],
                override val deinitTx: Option[DeinitTx],
            ) extends Minimal,
                  BlockEffects.MultiSigned.Final.Section {
                override def effects: BlockEffects.MultiSigned.Final =
                    BlockEffects.MultiSigned.Final(finalizationTx, rolloutTxs, deinitTx)
            }
        }

    }

    type Request = BlockConfirmed.Major | BlockConfirmed.Final | Timeout.type
    type Handle = ActorRef[IO, Request]

end CardanoLiaison

trait CardanoLiaison(
    config: CardanoLiaison.Config,
    pendingConnections: MultisigRegimeManager.PendingConnections | CardanoLiaison.Connections,
) extends Actor[IO, CardanoLiaison.Request]:
    import CardanoLiaison.*

    private val connections = Ref.unsafe[IO, Option[CardanoLiaison.Connections]](None)

    private val stateRef = Ref.unsafe[IO, CardanoLiaison.State](State.initialState(config))

    private def getConnections: IO[Connections] = this.connections.get.flatMap(
      _.fold(
        IO.raiseError(
          java.lang.Error(
            "Consensus Actor is missing its connections to other actors."
          )
        )
      )(IO.pure)
    )

    private def initializeConnections: IO[Unit] = pendingConnections match {
        case x: MultisigRegimeManager.PendingConnections =>
            for {
                _connections <- x.get
                _ <- connections.set(
                  Some(CardanoLiaison.Connections(blockWeaver = _connections.blockWeaver))
                )
            } yield ()
        case x: CardanoLiaison.Connections => connections.set(Some(x))
    }

    override def preStart: IO[Unit] =
        for {
            _ <- initializeConnections
            _ <- context.setReceiveTimeout(config.receiveTimeout, CardanoLiaison.Timeout)
        } yield ()

    override def receive: Receive[IO, Request] = {
        case block: BlockConfirmed.Major =>
            handleMajorBlockL1Effects(block) >> runEffects
        case block: BlockConfirmed.Final =>
            handleFinalBlockL1Effects(block) >> runEffects
        case CardanoLiaison.Timeout => IO.println("Timeout") >> runEffects
    }

    // ===================================
    // Inbox handlers
    // ===================================

    /** Handle [[Block.MultiSigned.Major]] request:
      *   - saves the effects in the internal actor's state
      */
    protected[consensus] def handleMajorBlockL1Effects(block: BlockConfirmed.Major): IO[Unit] =
        for {
            _ <- IO.println("handleMajorBlockL1Effects")
            _ <- stateRef.update(s => {
                val (blockEffectInputs, blockEffects) =
                    mkHappyPathEffectInputsAndEffects(block.settlementTx, block.rolloutTxs)
                State(
                  targetState = TargetState.Active(
                    UtxoIdL1(block.settlementTx.treasuryProduced.utxoId)
                  ),
                  effectInputs = s.effectInputs ++ blockEffectInputs,
                  happyPathEffects = s.happyPathEffects ++ blockEffects,
                  fallbackEffects = s.fallbackEffects + (block.blockNum -> block.fallbackTx)
                )
            })
        } yield ()

    /** Handle [[Block.MultiSigned.Final]] request:
      *   - saves the effects in the internal actor's state
      */
    protected[consensus] def handleFinalBlockL1Effects(block: BlockConfirmed.Final): IO[Unit] =
        for {
            _ <- IO.println("handleFinalBlockL1Effects")
            _ <- stateRef.update(s => {
                val (blockEffectInputs, blockEffects) =
                    mkHappyPathEffectInputsAndEffects(
                      block.finalizationTx,
                      block.rolloutTxs,
                      block.deinitTx
                    )
                s.copy(
                  targetState = TargetState.Finalized(block.finalizationTx.tx.id),
                  effectInputs = s.effectInputs ++ blockEffectInputs,
                  happyPathEffects = s.happyPathEffects ++ blockEffects
                )
            })
        } yield ()

    private def mkHappyPathEffectInputsAndEffects(
        settlementTx: SettlementTx,
        rollouts: List[RolloutTx]
    ): (
        Seq[(UtxoIdL1, EffectId)],
        Seq[(EffectId, HappyPathEffect)]
    ) = {
        val treasurySpent = settlementTx.treasurySpent
        val effects: List[(TransactionInput, HappyPathEffect)] =
            List(treasurySpent.utxoId -> settlementTx)
            // TODO: add utxoId
                ++ rollouts.map(r => r.rolloutSpent.utxo.input -> r)
        val blockNumber = BlockNumber(settlementTx.majorVersionProduced)
        indexWithEffectId(effects, blockNumber).unzip
    }

    private def indexWithEffectId(
        effects: List[(TransactionInput, HappyPathEffect)],
        blockNumber: BlockNumber
    ): List[((UtxoIdL1, EffectId), (EffectId, HappyPathEffect))] =
        effects.zipWithIndex
            .map((utxoIdAndEffect, index) => {
                val effectId = blockNumber -> index
                (UtxoIdL1(
                  utxoIdAndEffect._1
                ) -> effectId) -> (effectId -> utxoIdAndEffect._2)
            })

    private def mkHappyPathEffectInputsAndEffects(
        finalizationTx: FinalizationTx,
        rollouts: List[RolloutTx],
        mbDeinitTx: Option[DeinitTx]
    ): (
        Seq[(UtxoIdL1, EffectId)],
        Seq[(EffectId, HappyPathEffect)]
    ) =
        val treasurySpent = finalizationTx.treasurySpent
        val blockNumber = BlockNumber(finalizationTx.majorVersionProduced)

        val effects: List[(TransactionInput, HappyPathEffect)] =
            List(treasurySpent.utxoId -> finalizationTx)
            // TODO: add utxoId
                ++ rollouts.map(r => r.rolloutSpent.utxo.input -> r)

        val ret = indexWithEffectId(effects, blockNumber).unzip

        val deinitEffect = indexWithEffectId(
          mbDeinitTx.toList.map(d => d.residualTreasurySpent.utxoId -> d),
          blockNumber.increment
        ).unzip

        (ret._1 ++ deinitEffect._1, ret._2 ++ deinitEffect._2)

    /** The core part of the liaison that decides whether an action is needed and submits them.
      *
      * It's called either when:
      *   - the liaison learns a new effect
      *   - by receiving timeout
      */
    private def runEffects: IO[Unit] = for {

        _ <- IO.println("runEffects")

        // 1. Get the L1 state, i.e. the list of utxo ids at the multisig address  + the current time
        resp <- config.cardanoBackend.utxosAt(config.initializationTx.treasuryProduced.address)

        _ <- resp match {

            case Left(err) =>
                // This may happen if L1 API is temporarily unavailable or misconfigured
                // TODO: we need to address time when we work on autonomous mode
                //   but for now we can just ignore it and skip till the next event/timeout
                IO.println(s"error when getting Cardano L1 state: ${err}")

            case Right(l1State) =>
                for {
                    // From the whole state we need to know only utxo ids
                    utxoIds <- IO.pure(l1State.untagged.keySet)
                    // This may not the ideal place to have it. Every time we get a new head state, we
                    // forward it to the block weaver.
                    conn <- getConnections
                    _ <- conn.blockWeaver ! PollResults(utxoIds)

                    // 2. Based on the local state, find all due actions
                    state <- stateRef.get

                    // _ <- IO.println(state.effectInputs)
                    // _ <- IO.println(state.happyPathEffects.keys)
                    // _ <- IO.println(state.fallbackEffects.keys)

                    currentTime <- IO.realTime.map(_.toEpochQuantizedInstant(config.slotConfig))

                    // (i.e. those that are directly caused by effect inputs in L1 response).
                    dueActions: Seq[DirectAction] = mkDirectActions(
                      state,
                      utxoIds,
                      currentTime
                    ).fold(e => throw RuntimeException(e.msg), x => x)

                    // 3. Determine whether the initialization requires submission.
                    actionsToSubmit <-
                        // Empty direct actions is a precondition for the init (happy path) tx submission
                        if dueActions.nonEmpty
                        then IO.pure(dueActions)
                        else {

                            lazy val initAction = {
                                if currentTime < config.initializationTx.validityEnd
                                then
                                    Seq(
                                      Action.InitializeHead(
                                        state.happyPathEffects.values.map(_.tx).toSeq
                                      )
                                    )
                                else {
                                    Seq.empty
                                }
                            }
                            // TODO: check the rule-based treasury, and if it exists, don't try to initialize the head.
                            state.targetState match {
                                case TargetState.Active(targetTreasuryUtxoId) =>
                                    IO.pure(
                                      if utxoIds.contains(targetTreasuryUtxoId)
                                      then List.empty // everything is up-to-date on L1
                                      else initAction
                                    )
                                case TargetState.Finalized(finalizationTxHash) =>
                                    for {
                                        txResp <- config.cardanoBackend.getTxInfo(
                                          finalizationTxHash
                                        )
                                        mbInitAction <- txResp match {
                                            case Left(err) =>
                                                for {
                                                    _ <- IO.println(
                                                      s"error when getting finalization tx info: ${err}"
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

                    // 4. Submit flattened txs for actions it there are some
                    _ <- IO.println("\nLiaison's actions:")
                    _ <- actionsToSubmit.traverse_(a => IO.println(s"\t- ${a.msg}"))

                    submitRet <-
                        if actionsToSubmit.nonEmpty then
                            IO.traverse(actionsToSubmit.flatMap(actionTxs).toList)(
                              config.cardanoBackend.submitTx
                            )
                        else IO.pure(List.empty)

                    // Submission errors are ignored, but just dumped here
                    _ <- IO.println(submitRet)

                } yield ()
        }
    } yield ()

    // ===================================
    // Actions
    // ===================================

    /** The set of effects the actor may want to execute against L1. */
    sealed trait Action
    sealed trait DirectAction extends Action

    object Action {

        /** Switching into the rule-based regime. */
        final case class FallbackToRuleBased(tx: Transaction) extends DirectAction

        /** Pushing the existing state in the multisig regime forward. */
        final case class PushForwardMultisig(txs: Seq[Transaction]) extends DirectAction

        /** Finalizing a rollout sequence. */
        final case class Rollout(txs: Seq[Transaction]) extends DirectAction

        /** Represents noop action that may occur when the current time falls into the silence
          * period - a gap between two competing transactions when the settlement/finalization tx
          * already expired but the fallback is not valid yet.
          */
        case object SilencePeriodNoop extends DirectAction

        /** Like [[PushForwardMultisig]] but starting from the initialization tx. */
        final case class InitializeHead(txs: Seq[Transaction]) extends Action
    }

    private def actionTxs(action: Action): Seq[Transaction] = action match {
        case Action.FallbackToRuleBased(tx)  => Seq(tx)
        case Action.PushForwardMultisig(txs) => txs
        case Action.Rollout(txs)             => txs
        case Action.SilencePeriodNoop        => Seq.empty
        case Action.InitializeHead(txs)      => txs
    }

    extension (action: Action)

        private def msg: String =
            import Action.*
            action match {
                case FallbackToRuleBased(tx)  => s"FallbackToRuleBased (${tx.id})"
                case PushForwardMultisig(txs) => s"PushForwardMultisig (${txs.map(_.id)}"
                case Rollout(txs)             => s"Rollout (${txs.map(_.id)}"
                case SilencePeriodNoop        => "SilencePeriodNoop"
                case InitializeHead(txs)      => s"InitializeHead (${txs.map(_.id)}"
            }

    private def mkDirectActions(
        state: State,
        utxosFound: Set[UtxoIdL1],
        currentTime: QuantizedInstant
    ): Either[EffectError, Seq[DirectAction]] =
        utxosFound
            .map(state.effectInputs.get)
            .filter(_.isDefined)
            .map(_.get)
            .toSeq
            .sorted
            .map(mkDirectAction(state, currentTime))
            .sequence

    private def mkDirectAction(state: State, currentTime: QuantizedInstant)(
        effectId: EffectId
    ): Either[EffectError, DirectAction] = {
        import Action.*
        import EffectError.*

        effectId match {
            // Backbone effect - settlement/finalization/deinit
            // TODO: Can't be initialization tx though. If we want to allow
            // initialization txs to spend utxos from the same head address
            // we should address it somehow.
            case backboneEffectId @ (blockNum, 0) =>

                println(s"mkDirectAction: backboneEffectId: $backboneEffectId")

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
                                case tx: SettlementTx   => Right(tx.validityEnd)
                                case tx: FinalizationTx => Right(tx.validityEnd)
                                // TODO: this should never happen
                                case tx: InitializationTx =>
                                    Left(UnexpectedInitializationEffect(backboneEffectId))
                                case _: RolloutTx => Left(UnexpectedRolloutEffect(backboneEffectId))
                                case _: DeinitTx  => Left(DeinitEffectWithUnexpectedFallback)
                            }

                            fallbackValidityStart = fallback.validityStart

                            _ = println(
                              s"currentTime: $currentTime, happyPathTxTtl: $happyPathTxTtl, fallbackValidityStart: $fallbackValidityStart"
                            )

                            // Choose between (1), (2), and (3)
                            ret <- (
                              currentTime,
                              happyPathTxTtl,
                              fallbackValidityStart
                            ) match {
                                // (1)
                                case _ if currentTime < happyPathTxTtl =>
                                    val effectTxs =
                                        state.happyPathEffects
                                            .rangeFrom(backboneEffectId)
                                            .toSeq
                                            .map(_._2)
                                    Right(PushForwardMultisig(effectTxs.map(_.tx)))
                                // (2)
                                case _
                                    if currentTime >= happyPathTxTtl && currentTime < fallbackValidityStart =>
                                    Right(SilencePeriodNoop)
                                // (3)
                                case _ if currentTime >= fallbackValidityStart =>
                                    Right(
                                      FallbackToRuleBased(
                                        mbCompetingFallbackEffect.get.tx
                                      )
                                    )
                                // Should never happen, indicates an error in validity range calculation
                                case _ =>
                                    Left(
                                      WrongValidityRange(
                                        currentTime,
                                        happyPathTxTtl,
                                        fallbackValidityStart
                                      )
                                    )
                            }
                        } yield ret

                    // This is a deinit effect, always the last tx in the backbone
                    case None =>
                        println("-------> mbCompetingFallbackEffect == None")
                        val deinitTxEffectId = backboneEffectId
                        Right(PushForwardMultisig(Seq(state.happyPathEffects(deinitTxEffectId).tx)))
                }

            // Rollout tx
            case rolloutTx @ (blockNum, _notZero) =>
                println(s"mkDirectAction: rolloutEffectId: $rolloutTx")

                val nextBackboneTx = blockNum.increment -> 0
                val effectTxs =
                    state.happyPathEffects.range(rolloutTx, nextBackboneTx).toSeq.map(_._2)
                Right(Rollout(effectTxs.map(_.tx)))
        }
    }

    private enum EffectError extends Throwable:
        case UnexpectedRolloutEffect(effectId: EffectId)
        case UnexpectedInitializationEffect(effectId: EffectId)
        case DeinitEffectWithUnexpectedFallback
        case WrongValidityRange(
            currentTime: QuantizedInstant,
            happyPathTtl: QuantizedInstant,
            fallbackValidityStart: QuantizedInstant
        )

    import EffectError.*

    extension (self: EffectError)
        private def msg: String = self match {
            case UnexpectedRolloutEffect(effectId) =>
                s"Unexpected rollout effect with effectId = $effectId, check the integrity of effects."
            case UnexpectedInitializationEffect(effectId) =>
                s"Unexpected initialization effect with effectId = $effectId, check the integrity of effects and the initialization tx."
            case DeinitEffectWithUnexpectedFallback =>
                "Impossible: the deinit tx with a competing fallback tx"
            case WrongValidityRange(currentTime, happyPathTtl, fallbackValidityStart) =>
                s"Validity range invariant is not hold: current time: $currentTime," +
                    s" happy path tx TTL: $happyPathTtl" +
                    s" fallback validity start: $fallbackValidityStart"
        }

end CardanoLiaison
