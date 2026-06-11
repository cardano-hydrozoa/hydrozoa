package hydrozoa.multisig.consensus

import cats.effect.{IO, Ref}
import cats.implicits.*
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.ActorRef
import hydrozoa.config.head.initialization.InitialBlock
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.FallbackTxStartTime
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.node.operation.multisig.NodeOperationMultisigConfig
import hydrozoa.config.node.owninfo.OwnHeadPeerPublic
import hydrozoa.lib.cardano.scalus.QuantizedTime.{QuantizedInstant, toEpochQuantizedInstant}
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.MultisigRegimeManager
import hydrozoa.multisig.backend.cardano.CardanoBackend
import hydrozoa.multisig.consensus.pollresults.PollResults
import hydrozoa.multisig.ledger.block.BlockVersion
import hydrozoa.multisig.ledger.block.BlockVersion.Major.increment
import hydrozoa.multisig.ledger.l1.tx.*
import hydrozoa.multisig.ledger.stack.{PartitionEffects, Stack, StackEffects}
import scala.collection.immutable.{Seq, TreeMap}
import scala.math.Ordered.orderingToOrdered
import scalus.cardano.ledger.{Block as _, BlockHeader as _, Transaction, TransactionHash, TransactionInput}

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
  *     like initialization, settlement, rollback, or a fallback tx.
  *   - Every effect is tagged with an _effect id_ which is a pair: (major version, index). This
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
        cardanoBackend: CardanoBackend[IO],
        pendingConnections: MultisigRegimeManager.PendingConnections | CardanoLiaison.Connections,
        tracer: ContraTracer[IO, CardanoLiaisonEvent]
    ): IO[CardanoLiaison] =
        IO(new CardanoLiaison(config, cardanoBackend, pendingConnections, tracer) {})

    type Config = CardanoNetwork.Section & InitialBlock.Section &
        NodeOperationMultisigConfig.Section & OwnHeadPeerPublic.Section

    final case class Connections(
        blockWeaver: BlockWeaver.Handle
    )

    // ===================================
    // Actor's Internal state
    // ===================================

    /** The first part is major version, not block number, since having contigious numbering is
      * better.
      *
      * The second part of the EffectId is a number:
      *   - 0 - settlement
      *   - 1,2,3,... - rollouts
      *
      * For deinit we use phony "next major version", i.e. treat it as just extra backbone tx.
      */
    type EffectId = (BlockVersion.Major, Int)

    object EffectId:
        val initializationEffectId: EffectId = BlockVersion.Major.zero -> 0

    /** The state we want to achieve on L1. */
    enum TargetState:
        /** Regular state of an active head represented by id of the treasury utxo. */
        case Active(treasuryUtxoId: TransactionInput)

        /** Final state of a head, represented by the transaction hash of the finalization tx. */
        case Finalized(finalizationTxHash: TransactionHash)

    type HappyPathEffect = InitializationTx | SettlementTx | FinalizationTx | RolloutTx

    extension (effect: HappyPathEffect)
        def tx: Transaction = effect match
            case e: InitializationTx => e.tx
            case e: SettlementTx     => e.tx
            case e: FinalizationTx   => e.tx
            case e: RolloutTx        => e.tx

    /** Internal state of the actor. */
    final case class State(
        /** L1 target state */
        targetState: TargetState,

        /** Contains spent inputs mapping for all effects modulo the initialization tx, since
          * usually it doesn't spend any utxos locked at the head's address, and even if this is the
          * case, the initialization tx is handled separately.
          */
        effectInputs: Map[TransactionInput, EffectId],

        /** This contains all effects, the whole fish skeleton, including the initialization tx, but
          * with no fallback txs, which are stored separately in [[fallbackEffects]]
          */
        happyPathEffects: TreeMap[EffectId, HappyPathEffect],

        /** Fallback effects, indexed by the major version of block where they were created. */
        fallbackEffects: Map[BlockVersion.Major, FallbackTx]
    )

    object State:
        def initialState(config: Config): State = {
            // The submittable init + fallback effects are NOT seeded from config: those bodies are
            // the UNSIGNED genesis placeholders, never submittable. CardanoLiaison learns the real
            // (multisigned) init + fallback only from the hard-confirmed stack 0, via
            // `handleInitialStackL1Effects`. We only seed the target treasury utxo id, which is
            // identified by the init tx id (a body hash, witness-independent) and so is exact even
            // from the unsigned body — it tells the liaison what to look for on L1 before stack 0
            // hard-confirms (until then `happyPathEffects` is empty, so nothing is submitted).
            // TODO: I guess we don't need to set target state here, deferring to the persistent-recovery.
            State(
              targetState = TargetState.Active(config.initializationTx.treasuryProduced.utxoId),
              effectInputs = Map.empty,
              happyPathEffects = TreeMap.empty,
              fallbackEffects = Map.empty
            )
        }

        extension (state: State)
            def prettyDump: String = {
                val targetStateStr = state.targetState match {
                    case TargetState.Active(treasuryUtxoId) =>
                        s"Active(treasuryUtxoId=${treasuryUtxoId})"
                    case TargetState.Finalized(finalizationTxHash) =>
                        s"Finalized(txHash=${finalizationTxHash})"
                }

                val effectInputsStr = state.effectInputs
                    .map { case (txIn, effectId) =>
                        s"  ${txIn} -> ${effectId}"
                    }
                    .mkString("\n")

                val happyPathEffectsStr = state.happyPathEffects
                    .map { case (effectId, effect) =>
                        val txHash = effect.tx.id
                        s"  ${effectId} -> txHash=${txHash}"
                    }
                    .mkString("\n")

                val fallbackEffectsStr = state.fallbackEffects
                    .map { case (version, fallbackTx) =>
                        val txHash = fallbackTx.tx.id
                        s"  ${version} -> txHash=${txHash}"
                    }
                    .mkString("\n")

                s"""State(
                   |  targetState: ${targetStateStr}
                   |  effectInputs (${state.effectInputs.size} entries):
                   |${effectInputsStr}
                   |  happyPathEffects (${state.happyPathEffects.size} entries):
                   |${happyPathEffectsStr}
                   |  fallbackEffects (${state.fallbackEffects.size} entries):
                   |${fallbackEffectsStr}
                   |)""".stripMargin
            }

    // ===================================
    // Request + ActorRef + apply
    // ===================================
    object Timeout

    type Request =
        PreStart.type | Timeout.type | Stack.HardConfirmed
    type Handle = ActorRef[IO, Request]

    case object PreStart

end CardanoLiaison

trait CardanoLiaison(
    config: CardanoLiaison.Config,
    cardanoBackend: CardanoBackend[IO],
    pendingConnections: MultisigRegimeManager.PendingConnections | CardanoLiaison.Connections,
    tracer: ContraTracer[IO, CardanoLiaisonEvent],
) extends Actor[IO, CardanoLiaison.Request]:
    import CardanoLiaison.*

    private val connections = Ref.unsafe[IO, Option[CardanoLiaison.Connections]](None)

    private val stateRef = Ref.unsafe[IO, CardanoLiaison.State](State.initialState(config))

    private def getConnections: IO[Connections] = this.connections.get.flatMap(
      _.fold(
        IO.raiseError(
          RuntimeException("Consensus Actor is missing its connections to other actors.")
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
        context.self ! CardanoLiaison.PreStart

    override def receive: Receive[IO, Request] = PartialFunction.fromFunction(receiveTotal)

    private def receiveTotal(req: Request): IO[Unit] =
        req match {
            case CardanoLiaison.PreStart =>
                preStartLocal
            case CardanoLiaison.Timeout =>
                tracer.traceWith(CardanoLiaisonEvent.TimeoutReceived) >> runEffects
            case stack: Stack.HardConfirmed =>
                // The MULTISIGNED effects: SlowConsensusActor has aggregated every head
                // peer's hard-ack signature into VKeyWitnesses and attached them onto
                // these tx bodies, so they are L1-submittable as is (NOT
                // `unsigned.effects`, which are the unwitnessed bodies).
                val effects = stack.effects
                tracer.traceWith(
                  CardanoLiaisonEvent.StackHardConfirmedReceived(stack.brief.stackNum)
                ) >> (effects match {
                    case ini: StackEffects.HardConfirmed.Initial =>
                        // Stack 0 (initial). We MUST submit the initialization tx from the
                        // hard-confirmed initial stack — NOT `config.initializationTx`,
                        // which is the UNSIGNED body from head config.
                        handleInitialStackL1Effects(ini) >> runEffects
                    case reg: StackEffects.HardConfirmed.Regular =>
                        // Learn the stack's effects, then run the submission state
                        // machine immediately.
                        handleStackL1Effects(reg) >> runEffects
                })
        }

    private def preStartLocal: IO[Unit] =
        for {
            _ <- initializeConnections
            // Immediate + periodic Timeout
            _ <- context.self ! CardanoLiaison.Timeout
            _ <- context.setReceiveTimeout(
              config.cardanoLiaisonPollingPeriod,
              CardanoLiaison.Timeout
            )
        } yield ()

    // ===================================
    // Inbox handlers
    // ===================================

    /** Learn the hard-confirmed *initial* stack's L1 effects (stack 0).
      *
      * `State.initialState` seeds `EffectId.initializationEffectId` from `config.initializationTx`
      * and `fallbackEffects(Major.zero)` from `config.initialFallbackTx`. That config init tx is
      * the **unsigned** body — usable only as a pre-confirmation placeholder, never submittable.
      * Once stack 0 is hard-confirmed, [[StackEffects.HardConfirmed.Initial]] carries the
      * slow-consensus-ratified init tx (the round-2 Initial unlock) + the locally-derived fallback;
      * this overrides the seeded entries with them so `runEffects` submits the correct init tx.
      *
      * As on the [[handleStackL1Effects]] (Regular) path, `eff`'s init tx + fallback bodies are
      * already MULTISIGNED — SlowConsensusActor aggregated the saturated round-1/round-2 Initial
      * hard-acks (including each peer's individual funding witnesses) into the witnesses on these
      * bodies. The remaining Initial-only gap is *Bootstrap wiring* — how stack 0 gets
      * *composed/produced* in the first place (StackComposer's `Bootstrap` param) — which is
      * orthogonal to (and unaffected by) this witnessing.
      */
    private def handleInitialStackL1Effects(eff: StackEffects.HardConfirmed.Initial): IO[Unit] =
        for {
            _ <- tracer.traceWith(CardanoLiaisonEvent.InitialStackEffectsLearned)
            newState <- stateRef.updateAndGet { s =>
                s.copy(
                  targetState = TargetState.Active(eff.initializationTx.treasuryProduced.utxoId),
                  happyPathEffects = s.happyPathEffects
                      .updated(EffectId.initializationEffectId, eff.initializationTx),
                  fallbackEffects =
                      s.fallbackEffects.updated(BlockVersion.Major.zero, eff.fallbackTx)
                )
            }
            _ <- tracer.traceWith(CardanoLiaisonEvent.InitialStackEffectsState(newState.prettyDump))
        } yield ()

    /** Learn a hard-confirmed stack's L1 effects into the submission state machine.
      *
      * Translates [[StackEffects.HardConfirmed.Regular]] into the `(effectInputs, happyPathEffects,
      * fallbackEffects, targetState)` shape the `runEffects` / `mkDirectActions` machinery
      * consumes, so effects submit in dependency order (backbone first via `EffectId (major, 0)`,
      * then its rollouts `(major, 1..n)`; competing fallback resolved by
      * `fallbackEffects.get(major.decrement)`).
      *
      * Backbones (settlements, then the optional finalization) are walked in stack/major order;
      * each contributes one `EffectId` family keyed by its `majorVersionProduced`. Fallbacks are
      * keyed by their settlement's `majorVersionProduced`, so the NEXT settlement finds its
      * competing fallback via `major.decrement`.
      *
      * NOT submitted here (intentional, per spec):
      *   - `evacCommit` — a dormant dispute-only record, never an immediate L1 tx (presented to the
      *     rules-based dispute scripts only after a fallback; the consensus artifact is the header
      *     signature in the hard-ack, persisted by the future storage layer).
      *   - `refunds` — post-dated refund txs are not submitted here; refund-tx L1 submission is
      *     deferred (see `StackEffectsBuilder.finalizeLedger` TODO fund14).
      *
      * Minor-only stacks (no settlement, no finalization) carry no backbone — nothing reaches L1;
      * `targetState` is left unchanged.
      *
      * The effect bodies in `eff` are already MULTISIGNED: SlowConsensusActor aggregates every head
      * peer's verified hard-ack signature into `VKeyWitness`es and attaches them onto each effect
      * tx before emitting `Stack.HardConfirmed.effects` (same for the Initial path, see
      * [[handleInitialStackL1Effects]]). So they are submittable on L1 as is — no
      * witness-attachment step remains here.
      */
    private def handleStackL1Effects(eff: StackEffects.HardConfirmed.Regular): IO[Unit] = {
        val parts = eff.partitions.toList
        // A minor-only stack (only Minor partitions) produces no L1 backbone effect — nothing to
        // learn or submit — so short-circuit before deriving anything or touching the state.
        // TODO: teach SlowConsensusActor not to emit minor-only stacks at all, so CardanoLiaison
        //   never has to receive (and skip) them.
        val hasBackbone = parts.exists {
            case _: PartitionEffects.Major[?] => true
            case _: PartitionEffects.Final    => true
            case _: PartitionEffects.Minor[?] => false
        }
        if !hasBackbone then tracer.traceWith(CardanoLiaisonEvent.MinorOnlyStackReceived)
        else {
            // Flatten the partition-indexed effects into the (settlements, fallbacks, rollouts,
            // finalization) the submission state machine consumes. Each Major partition carries
            // its own settlement+fallback together (so `settlements.zip(fallbacks)` aligns
            // exactly); rollouts come from every Major / Final partition; refunds + SEC are NOT
            // submitted.
            val settlements: List[SettlementTx] =
                parts.collect { case PartitionEffects.Major(s, _, _, _, _) => s }
            val fallbacks: List[FallbackTx] =
                parts.collect { case PartitionEffects.Major(_, f, _, _, _) => f }
            val finalization: Option[FinalizationTx] =
                parts.collectFirst { case PartitionEffects.Final(f, _) => f }
            val allRollouts: List[RolloutTx] = parts.flatMap {
                case PartitionEffects.Major(_, _, ro, _, _) => ro
                case PartitionEffects.Final(_, ro)          => ro
                case PartitionEffects.Minor(_, _)           => Nil
            }
            val backbones: List[SettlementTx | FinalizationTx] =
                settlements ++ finalization.toList

            // Accumulate every state delta up front — each is a pure function of the flattened
            // effects, independent of the current state — then apply them all in a single `copy`
            // below.
            val perBackbone =
                backbones.map(b =>
                    mkHappyPathEffectInputsAndEffects(b, rolloutsFor(b, allRollouts))
                )
            val newEffectInputs: Seq[(TransactionInput, EffectId)] = perBackbone.flatMap(_._1)
            val newHappyPathEffects: Seq[(EffectId, HappyPathEffect)] = perBackbone.flatMap(_._2)
            val newFallbackEffects: Seq[(BlockVersion.Major, FallbackTx)] =
                settlements.zip(fallbacks).map((s, f) => s.majorVersionProduced -> f)
            // Always `Some` here (a backbone exists): finalization → Finalized, else the last
            // settlement's treasury → Active.
            val newTarget: Option[TargetState] = finalization match {
                case Some(fin) => Some(TargetState.Finalized(fin.tx.id))
                case None =>
                    settlements.lastOption.map(s => TargetState.Active(s.treasuryProduced.utxoId))
            }

            for {
                _ <- tracer.traceWith(
                  CardanoLiaisonEvent.StackEffectsLearned(
                    settlements.size,
                    fallbacks.size,
                    allRollouts.size,
                    finalization.isDefined
                  )
                )
                newState <- stateRef.updateAndGet { s =>
                    s.copy(
                      effectInputs = s.effectInputs ++ newEffectInputs,
                      happyPathEffects = s.happyPathEffects ++ newHappyPathEffects,
                      fallbackEffects = s.fallbackEffects ++ newFallbackEffects,
                      targetState = newTarget.getOrElse(s.targetState)
                    )
                }
                _ <- tracer.traceWith(CardanoLiaisonEvent.StackEffectsState(newState.prettyDump))
            } yield ()
        }
    }

    /** The rollout txs belonging to one backbone (settlement / finalization), in chain order.
      *
      * `StackEffects.HardConfirmed.Regular.rollouts` is flat across the whole stack; a backbone
      * owns the rollout chain that starts at the rollout utxo it produces (`mbRolloutProduced`),
      * each subsequent rollout spending the previous one's produced rollout utxo until a
      * [[RolloutTx.Last]]. Linking by `utxo.input` is independent of the flat list's order and
      * mirrors how `mkDirectAction` itself walks the chain on L1.
      */
    private def rolloutsFor(
        backbone: SettlementTx | FinalizationTx,
        all: List[RolloutTx]
    ): List[RolloutTx] = {
        @annotation.tailrec
        def chain(spentInput: TransactionInput, acc: List[RolloutTx]): List[RolloutTx] =
            all.find(_.rolloutSpent.utxo.input == spentInput) match {
                case None => acc.reverse
                case Some(r: RolloutTx.NotLast) =>
                    chain(r.rolloutProduced.utxo.input, r :: acc)
                case Some(r) => (r :: acc).reverse // RolloutTx.Last
            }
        backbone.mbRolloutProduced match {
            case None    => Nil
            case Some(u) => chain(u.utxo.input, Nil)
        }
    }

    private def mkHappyPathEffectInputsAndEffects(
        majorTx: SettlementTx | FinalizationTx,
        rollouts: List[RolloutTx]
    ): (
        Seq[(TransactionInput, EffectId)],
        Seq[(EffectId, HappyPathEffect)]
    ) = {
        val treasurySpent = majorTx.treasurySpent

        val effects: List[(TransactionInput, HappyPathEffect)] =
            List(treasurySpent.utxoId -> majorTx)
            // TODO: implement utxoId?
                ++ rollouts.map(r => r.rolloutSpent.utxo.input -> r)
        indexWithEffectId(effects, majorTx.majorVersionProduced).unzip
    }

    private def indexWithEffectId(
        effects: List[(TransactionInput, HappyPathEffect)],
        versionMajor: BlockVersion.Major
    ): List[((TransactionInput, EffectId), (EffectId, HappyPathEffect))] =
        effects.zipWithIndex
            .map((utxoIdAndEffect, index) => {
                val effectId = versionMajor -> index

                utxoIdAndEffect._1
                    -> effectId -> (effectId -> utxoIdAndEffect._2)
            })

    /** The core part of the liaison that decides whether an action is needed and submits them.
      *
      * It's called either when:
      *   - the liaison learns a new effect
      *   - by receiving timeout
      */
    private def runEffects: IO[Unit] = for {
        _ <- tracer.traceWith(CardanoLiaisonEvent.RunEffectsStarted)
        // 1. Get the L1 state, i.e. the list of utxo ids at the multisig address  + the current time
        resp <- cardanoBackend.utxosAt(config.initializationTx.treasuryProduced.address)

        _ <- resp match {

            case Left(err) =>
                // This may happen if L1 API is temporarily unavailable or misconfigured
                // TODO: we need to address time when we work on autonomous mode
                //   but for now we can just ignore it and skip till the next event/timeout
                tracer.traceWith(CardanoLiaisonEvent.L1StateQueryError(err.toString))

            case Right(l1State) =>
                for {
                    // From the whole state we need to know only utxo ids
                    utxoIds <- IO.pure(l1State.keySet)
                    // This may not the ideal place to have it. Every time we get a new head state, we
                    // forward it to the block weaver.
                    conn <- getConnections
                    _ <- conn.blockWeaver ! PollResults(utxoIds)

                    // 2. Based on the local state, find all due actions
                    state <- stateRef.get

                    currentTime <- IO.realTime.map(_.toEpochQuantizedInstant(config.slotConfig))

                    _ <- tracer.traceWith(
                      CardanoLiaisonEvent.CurrentL1State(
                        currentTime.toString,
                        utxoIds.mkString(","),
                        state.prettyDump
                      )
                    )

                    // (i.e. those that are directly caused by effect inputs in L1 response).
                    dueActions: Seq[DirectAction] <- mkDirectActions(
                      state,
                      utxoIds,
                      currentTime
                    ).fold(
                      e =>
                          tracer.traceWith(CardanoLiaisonEvent.CriticalError(e.msg)) >>
                              IO.raiseError(RuntimeException(e.msg)),
                      IO.pure
                    )
                    // .fold(e => {throw RuntimeException(e.msg)}, x => x)

                    actionsToSubmit <-
                        if dueActions.nonEmpty
                        then IO.pure(dueActions)
                        else
                            for {

                                _ <- tracer.traceWith(CardanoLiaisonEvent.NoActionsScheduled)

                                // Empty direct actions indicate another actions should be considered:
                                //  - the last fallback tx might have become valid
                                //  - the init (whole happy path) tx submission might be needed

                                // TODO: this is done in a bit a makeshift manner to fix the test, likely we want to do it
                                //   more systematically
                                lastFallback: Option[Transaction] = for {
                                    maxKey <- state.fallbackEffects.keySet.maxOption
                                    fallbackTx = state.fallbackEffects(maxKey)
                                    if utxoIds.contains(
                                      fallbackTx.treasurySpent.utxoId
                                    ) && fallbackTx.fallbackTxStartTime.convert <= currentTime
                                } yield fallbackTx.tx

                                ret <- lastFallback match {
                                    case Some(fallback) =>
                                        IO.pure(Seq(Action.FallbackToRuleBased(fallback)))
                                    case None => {
                                        lazy val initAction = {
                                            if currentTime < config.initializationTx.initializationTxEndTime.convert
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
                                                if utxoIds.contains(targetTreasuryUtxoId)
                                                then
                                                    // everything is up-to-date on L1
                                                    tracer.traceWith(
                                                      CardanoLiaisonEvent.TargetUtxoStatus(
                                                        targetTreasuryUtxoId.toString,
                                                        found = true
                                                      )
                                                    ) >> IO.pure(List.empty)
                                                else
                                                    tracer.traceWith(
                                                      CardanoLiaisonEvent.TargetUtxoStatus(
                                                        targetTreasuryUtxoId.toString,
                                                        found = false
                                                      )
                                                    ) >> IO.pure(initAction)

                                            case TargetState.Finalized(finalizationTxHash) =>
                                                for {
                                                    txResp <- cardanoBackend.isTxKnown(
                                                      finalizationTxHash
                                                    )
                                                    mbInitAction <- txResp match {
                                                        case Left(err) =>
                                                            tracer.traceWith(
                                                              CardanoLiaisonEvent
                                                                  .FinalizationTxQueryError(
                                                                    err.toString
                                                                  )
                                                            ) >> IO.pure(Seq.empty)
                                                        case Right(isKnown) =>
                                                            tracer.traceWith(
                                                              CardanoLiaisonEvent
                                                                  .FinalizationTxStatus(
                                                                    finalizationTxHash.toString,
                                                                    if isKnown then "known"
                                                                    else "not known"
                                                                  )
                                                            ) >> IO.pure(
                                                              if isKnown then Seq.empty
                                                              else initAction
                                                            )
                                                    }
                                                } yield mbInitAction
                                        }
                                    }
                                }
                            } yield ret

                    // 4. Submit flattened txs for actions it there are some
                    _ <- IO.whenA(actionsToSubmit.nonEmpty) {
                        val hasFallback =
                            actionsToSubmit.exists(action =>
                                action.isInstanceOf[Action.FallbackToRuleBased] ||
                                    action.isInstanceOf[Action.SilencePeriodNoop]
                            )
                        tracer.traceWith(
                          CardanoLiaisonEvent.ActionsDispatched(
                            actionsToSubmit.map(_.msg).toList,
                            hasFallback
                          )
                        )
                    }

                    submitRet <-
                        if actionsToSubmit.nonEmpty then
                            IO.traverse(actionsToSubmit.flatMap(actionTxs).toList)(tx =>
                                for {
                                    _ <- tracer.traceWith(
                                      CardanoLiaisonEvent.TxSubmitting(tx.id.toString)
                                    )
                                    ret <- cardanoBackend.submitTx(tx)
                                } yield tx -> ret
                            )
                        else IO.pure(List.empty)

                    // Submission errors are ignored, but dumped here
                    submissionErrors = submitRet.filter(_._2.isLeft)
                    _ <- IO.whenA(submissionErrors.nonEmpty)(
                      tracer.traceWith(CardanoLiaisonEvent.SubmissionErrors(submissionErrors.size))
                    )

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
        final case class SilencePeriodNoop(
            currentTime: QuantizedInstant,
            happyPathTxTtl: QuantizedInstant,
            fallbackValidityStart: FallbackTxStartTime
        ) extends DirectAction {}

        /** Like [[PushForwardMultisig]] but starting from the initialization tx. */
        final case class InitializeHead(txs: Seq[Transaction]) extends Action
    }

    private def actionTxs(action: Action): Seq[Transaction] = action match {
        case Action.FallbackToRuleBased(tx)    => Seq(tx)
        case Action.PushForwardMultisig(txs)   => txs
        case Action.Rollout(txs)               => txs
        case Action.SilencePeriodNoop(_, _, _) => Seq.empty
        case Action.InitializeHead(txs)        => txs
    }

    extension (action: Action)

        private def msg: String =
            import Action.*
            action match {
                case FallbackToRuleBased(tx)         => s"FallbackToRuleBased (${tx.id})"
                case PushForwardMultisig(txs)        => s"PushForwardMultisig (${txs.map(_.id)}"
                case Rollout(txs)                    => s"Rollout (${txs.map(_.id)}"
                case sp @ SilencePeriodNoop(_, _, _) => s"$sp"
                case InitializeHead(txs)             => s"InitializeHead (${txs.map(_.id)}"
            }

    private def mkDirectActions(
        state: State,
        utxosFound: Set[TransactionInput],
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
            // Backbone effect - settlement/finalization
            // TODO: Can't be initialization tx though. If we want to allow
            //   initialization txs to spend utxos from the same head address
            //   we should address it somehow.
            case backboneEffectId @ (versionMajor, 0) =>

                // println(s"mkDirectAction: backboneEffectId: $backboneEffectId")

                val happyPathEffect = state.happyPathEffects(backboneEffectId)
                val mbCompetingFallbackEffect = state.fallbackEffects.get(versionMajor.decrement)

                // Invariant: there should be always one sensible outcome:
                // - (1) either the settlement/finalization tx for block N+1 is valid
                // - (2) or we are inside the silence period
                // - (3) or the fallback tx for N is valid

                mbCompetingFallbackEffect match {

                    // This is the only correct case.
                    // TODO: ensure this always holds by construction
                    case Some(fallback) =>
                        for {
                            happyPathTxTtl: QuantizedInstant <- happyPathEffect match {
                                case tx: SettlementTx =>
                                    Right {
                                        val quantizedInstant: QuantizedInstant =
                                            tx.settlementTxEndTime.convert
                                        quantizedInstant
                                    }
                                case tx: FinalizationTx =>
                                    Right {
                                        val quantizedInstant: QuantizedInstant =
                                            tx.finalizationTxEndTime.convert
                                        quantizedInstant
                                    }
                                // TODO: this should never happen
                                case tx: InitializationTx =>
                                    Left(UnexpectedInitializationEffect(backboneEffectId))
                                case _: RolloutTx => Left(UnexpectedRolloutEffect(backboneEffectId))
                            }

                            fallbackValidityStart = fallback.fallbackTxStartTime

                            // _ = println(
                            //  s"currentTime: $currentTime, happyPathTxTtl: $happyPathTxTtl, fallbackValidityStart: $fallbackValidityStart"
                            // )

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
                                    // println(s"effectTxs.size=${effectTxs.size}")
                                    Right(PushForwardMultisig(effectTxs.map(_.tx)))
                                // (2)
                                case _
                                    if currentTime >= happyPathTxTtl && currentTime < fallbackValidityStart =>
                                    Right(
                                      SilencePeriodNoop(
                                        currentTime,
                                        happyPathTxTtl,
                                        fallbackValidityStart
                                      )
                                    )
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

                    // This should not be possible -- every non-initialization tx has a competing fallback tx.
                    case None =>
                        println("-------> mbCompetingFallbackEffect == None")
                        Left(MissingCompetingFallback(backboneEffectId))
                }

            // Rollout tx
            case rolloutTx @ (versionMajor, _notZero) =>
                // println(s"mkDirectAction: rolloutEffectId: $rolloutTx")

                val nextBackboneTx = versionMajor.increment -> 0
                val effectTxs =
                    state.happyPathEffects.range(rolloutTx, nextBackboneTx).toSeq.map(_._2)
                Right(Rollout(effectTxs.map(_.tx)))
        }
    }

    private enum EffectError extends Throwable:
        case UnexpectedRolloutEffect(effectId: EffectId)
        case UnexpectedInitializationEffect(effectId: EffectId)
        case MissingCompetingFallback(effectId: EffectId)
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
            case MissingCompetingFallback(effectId) =>
                s"Impossible: a settlement/finalization effect ($effectId) without a competing fallback tx."
            case WrongValidityRange(currentTime, happyPathTtl, fallbackValidityStart) =>
                s"Validity range invariant is not hold: current time: $currentTime," +
                    s" happy path tx TTL: $happyPathTtl" +
                    s" fallback validity start: $fallbackValidityStart"
        }

end CardanoLiaison
