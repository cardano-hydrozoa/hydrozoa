package hydrozoa.multisig.consensus

import cats.effect.{IO, Ref}
import cats.implicits.*
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.ActorRef
import hydrozoa.config.HydrozoaBlueprint
import hydrozoa.config.head.initialization.InitializationParameters
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.FallbackTxStartTime
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.config.node.operation.multisig.NodeOperationMultisigConfig
import hydrozoa.config.node.owninfo.OwnPeerPublic
import hydrozoa.lib.cardano.scalus.QuantizedTime.{QuantizedInstant, toEpochQuantizedInstant}
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.backend.cardano.CardanoBackend
import hydrozoa.multisig.consensus.pollresults.PollResults
import hydrozoa.multisig.ledger.block.BlockVersion
import hydrozoa.multisig.ledger.block.BlockVersion.Major.increment
import hydrozoa.multisig.ledger.l1.tx.*
import hydrozoa.multisig.ledger.stack.{PartitionEffects, Stack, StackEffects, StackNumber}
import hydrozoa.multisig.persistence.Persistence
import hydrozoa.multisig.persistence.recovery.HardConfirmationScan
import hydrozoa.multisig.{HeadMultisigRegimeManager, NodeStatus}
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
  * ==Decision schema (one pass of `runEffects`, per poll/timeout)==
  *
  * Every pass first polls the head multisig address for its utxo set, forwards it to BlockWeaver,
  * then walks the following ladder, taking the first branch that applies. All L1 reads beyond the
  * one address poll (steps 2–4) are made lazily — only on the branch that needs them — so a head
  * that is in sync makes exactly one Cardano query per pass.
  *
  *   1. '''Due effects at the multisig address.''' For each polled utxo, look up both the
  *      happy-path entry point (`state.happyPathSkeletonEntryPoints`) and the fallback that spends
  *      the same treasury utxo (`state.fallbackEffects`). A treasury's happy tx and its fallback
  *      have disjoint validity windows separated by a silence period, so submit whichever is valid
  *      now — push the backbone forward (`PushForwardMultisig`), submit the now-valid fallback
  *      (`FallbackToRuleBased`), or, in the gap, do nothing (`SilencePeriodNoop`); rollout inputs
  *      push their `Rollout`. This is the multisig-regime path — the happy path plus the
  *      timing-driven fallback submission (not purely happy-path).
  *   2. '''Finalization reached''' (only when the target is `Finalized`). If the finalization tx is
  *      on L1, the head is settled for good — nothing to do.
  *   3. '''Rule-based treasury present.''' If the target anchor (treasury utxo / finalization tx)
  *      is gone, probe the rule-based treasury beacon at its script address. Its presence means the
  *      head has fallen into the rule-based regime — send
  *      [[HeadMultisigRegimeManager.HandoffToRuleBased]] (idempotent; every peer that observes it
  *      hands off) and submit nothing. The rule-based side reads its version and status from this
  *      on-chain treasury utxo, so no tx needs to be carried.
  *   4. '''Resubmit the skeleton.''' No rule-based treasury either ⇒ a genuine L1 rollback. If the
  *      init tx is no longer on L1 the head must be re-established: resubmit the whole skeleton
  *      (`InitializeHead`), but only while inside the init tx's validity window (else
  *      `InitWindowElapsed`). If the init tx still stands, the head is up and step 1 alone repairs
  *      it as inputs reappear — nothing is resubmitted here.
  *
  * Submitting a fallback (step 1) and handing off to rule-based (step 3) are deliberately
  * decoupled: a peer first waits out the silence period and submits a suitable fallback, and only
  * on a later pass — once that fallback has landed and produced the rule-based treasury — does the
  * treasury-beacon probe fire the handoff.
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
        pendingConnections: HeadMultisigRegimeManager.PendingConnections |
            CardanoLiaison.Connections,
        tracer: ContraTracer[IO, CardanoLiaisonEvent],
        persistence: Persistence[IO],
        mrmSelf: ActorRef[IO, HeadMultisigRegimeManager.HandoffToRuleBased.type],
        advanceNodeStatus: NodeStatus => IO[Unit],
    ): IO[CardanoLiaison] =
        IO(
          new CardanoLiaison(
            config,
            cardanoBackend,
            pendingConnections,
            tracer,
            persistence,
            mrmSelf,
            advanceNodeStatus
          ) {}
        )

    type Config = CardanoNetwork.Section & NodeOperationMultisigConfig.Section &
        OwnPeerPublic.Section & HeadPeers.Section & InitializationParameters.Section

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
        /** The cold, pre-stack-0 target: the head is not yet on L1 and nothing is submittable. The
          * real target is learned from the hard-confirmed stack 0 (see
          * [[State.applyInitialEffects]]).
          */
        case Uninitialized

        /** Regular state of an active head represented by id of the treasury utxo. */
        case Active(treasuryUtxoId: TransactionInput)

        /** Final state of a head, represented by the transaction hash of the finalization tx. */
        case Finalized(finalizationTxHash: TransactionHash)

    /** Thrown by [[State.applyRegularEffects]] when a learned backbone and the fallback for the
      * treasury utxo it spends do not have disjoint validity windows (`happyPathTtl >
      * fallbackValidityStart`) — a bad tx-timing config that would let both be valid on L1 at once.
      * The live learn path traces [[CardanoLiaisonEvent.DisjointWindowViolation]] and re-raises to
      * stop; on `recover` it fails boot.
      */
    final case class DisjointWindowViolation(
        treasuryUtxo: TransactionInput,
        happyPathTtl: QuantizedInstant,
        fallbackValidityStart: QuantizedInstant
    ) extends RuntimeException(
          s"Disjoint-window invariant violated for treasury $treasuryUtxo: happy-path TTL " +
              s"$happyPathTtl > fallback validity start $fallbackValidityStart — the " +
              "settlement/finalization and its fallback would be valid on L1 at once (bad tx timing)."
        )

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

        /** Entry points into the happy-path skeleton, keyed by the L1 utxo whose appearance makes
          * an effect due: each maps a spent input to the [[EffectId]] that consumes it (backbones
          * by their treasury input, rollouts by their rollout input). Excludes the initialization
          * tx, since it usually spends no utxo locked at the head's address and is handled
          * separately.
          */
        happyPathSkeletonEntryPoints: Map[TransactionInput, EffectId],

        /** This contains all effects, the whole fish skeleton, including the initialization tx, but
          * with no fallback txs, which are stored separately in [[fallbackEffects]]
          */
        happyPathEffects: TreeMap[EffectId, HappyPathEffect],

        /** Fallback effects, keyed by the treasury utxo the fallback spends
          * (`fallbackTx.treasurySpent.utxoId`) — the same treasury utxo the next settlement
          * consumes via [[happyPathSkeletonEntryPoints]]. This lets `mkDirectActions` resolve a
          * treasury's happy continuation and its fallback under a single utxo key.
          */
        fallbackEffects: Map[TransactionInput, FallbackTx]
    )

    object State:
        /** The cold, pre-stack-0 state: no L1 target and no effects. The live `stateRef` boots here
          * and [[recover]] folds from here; [[applyInitialEffects]] instals the real target +
          * init/fallback once stack 0 hard-confirms. Config-free — the (multisigned) init +
          * fallback and the target treasury utxo id are all learned from the hard-confirmed stack
          * 0, never seeded from the unsigned config bodies, so nothing is submittable until then.
          */
        val empty: State =
            State(
              targetState = TargetState.Uninitialized,
              happyPathSkeletonEntryPoints = Map.empty,
              happyPathEffects = TreeMap.empty,
              fallbackEffects = Map.empty
            )

        /** Apply a hard-confirmed *initial* stack's L1 effects — the pure transition shared by the
          * live `Stack.HardConfirmed` path and [[recover]]: instal the ratified (multisigned) init
          * tx + fallback from stack 0 into the cold [[empty]] state, setting the real `Active`
          * target. This is what makes the head submittable; before it, the state is
          * `Uninitialized`.
          */
        def applyInitialEffects(state: State, eff: StackEffects.HardConfirmed.Initial): State =
            state.copy(
              targetState = TargetState.Active(eff.initializationTx.treasuryProduced.utxoId),
              happyPathEffects = state.happyPathEffects
                  .updated(EffectId.initializationEffectId, eff.initializationTx),
              fallbackEffects =
                  state.fallbackEffects.updated(eff.fallbackTx.treasurySpent.utxoId, eff.fallbackTx)
            )

        /** Apply a hard-confirmed *regular* stack's L1 effects — the pure transition shared by the
          * live path and [[recover]]. A minor-only stack carries no backbone, so `state` is
          * returned unchanged; otherwise the partition effects fold into
          * `(happyPathSkeletonEntryPoints, happyPathEffects, fallbackEffects, targetState)`.
          */
        def applyRegularEffects(state: State, eff: StackEffects.HardConfirmed.Regular): State = {
            val parts = eff.partitions.toList
            val hasBackbone = parts.exists {
                case _: PartitionEffects.Major[?] => true
                case _: PartitionEffects.Final    => true
                case _: PartitionEffects.Minor[?] => false
            }
            if !hasBackbone then state
            else {
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
                val perBackbone =
                    backbones.map(b =>
                        mkHappyPathEffectInputsAndEffects(b, rolloutsFor(b, allRollouts))
                    )
                val newEntryPoints = perBackbone.flatMap(_._1)
                val newHappyPathEffects = perBackbone.flatMap(_._2)
                val newFallbackEffects =
                    fallbacks.map(f => f.treasurySpent.utxoId -> f)
                // Fail loudly if a learned backbone and the fallback for the treasury it spends do
                // not have disjoint validity windows (a bad tx-timing config), rather than silently
                // producing an on-chain overlap that `mkDirectActions` would resolve at poll time.
                requireDisjointValidityWindows(
                  backbones,
                  state.fallbackEffects ++ newFallbackEffects
                )
                val newTarget: Option[TargetState] = finalization match {
                    case Some(fin) => Some(TargetState.Finalized(fin.tx.id))
                    case None =>
                        settlements.lastOption.map(s =>
                            TargetState.Active(s.treasuryProduced.utxoId)
                        )
                }
                state.copy(
                  happyPathSkeletonEntryPoints =
                      state.happyPathSkeletonEntryPoints ++ newEntryPoints,
                  happyPathEffects = state.happyPathEffects ++ newHappyPathEffects,
                  fallbackEffects = state.fallbackEffects ++ newFallbackEffects,
                  targetState = newTarget.getOrElse(state.targetState)
                )
            }
        }

        /** Reconstruct the submission state after a crash by folding every persisted
          * `Cf.HardConfirmation` (ascending stack order) through the same kernels the live
          * `Stack.HardConfirmed` path uses — so a recovered `State` equals a live run's. Submission
          * progress is **not** restored: `runEffects` re-samples L1 (§5.5), so only the effect
          * index is rebuilt. An empty CF folds to [[empty]] (the cold value); no `Option` (there is
          * no own-ack to gate on).
          */
        def recover(persistence: Persistence[IO])(using
            CardanoNetwork.Section
        ): IO[State] =
            HardConfirmationScan.scanFrom(persistence.backend, StackNumber.zero).map {
                _.foldLeft(empty) { (state, eff) =>
                    eff match {
                        case ini: StackEffects.HardConfirmed.Initial =>
                            applyInitialEffects(state, ini)
                        case reg: StackEffects.HardConfirmed.Regular =>
                            applyRegularEffects(state, reg)
                    }
                }
            }

        /** The rollout txs belonging to one backbone (settlement / finalization), in chain order.
          *
          * `StackEffects.HardConfirmed.Regular.rollouts` is flat across the whole stack; a backbone
          * owns the rollout chain that starts at the rollout utxo it produces
          * (`mbRolloutProduced`), each subsequent rollout spending the previous one's produced
          * rollout utxo until a [[RolloutTx.Last]]. Linking by `utxo.input` is independent of the
          * flat list's order and mirrors how `mkDirectAction` itself walks the chain on L1.
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

        /** Learn-time check of the disjoint-validity-window invariant: for each newly-learned
          * backbone, the fallback keyed at the treasury utxo the backbone spends must not open its
          * window before the backbone's own window ends (`happyTtl <= fallbackStart`) — else both
          * would be valid on L1 at once. The fallback is born with the block that produced the
          * treasury, so it is already keyed here when the next backbone spending that treasury is
          * learned. Throws [[DisjointWindowViolation]] on the first offending pair.
          */
        private def requireDisjointValidityWindows(
            backbones: List[SettlementTx | FinalizationTx],
            fallbacks: Map[TransactionInput, FallbackTx]
        ): Unit =
            backbones.foreach { backbone =>
                fallbacks.get(backbone.treasurySpent.utxoId).foreach { fallback =>
                    val happyTtl: QuantizedInstant = backbone match {
                        case tx: SettlementTx   => tx.settlementTxEndTime.convert
                        case tx: FinalizationTx => tx.finalizationTxEndTime.convert
                    }
                    val fallbackStart: QuantizedInstant = fallback.fallbackTxStartTime.convert
                    if happyTtl > fallbackStart then
                        throw DisjointWindowViolation(
                          backbone.treasurySpent.utxoId,
                          happyTtl,
                          fallbackStart
                        )
                }
            }

        extension (state: State)
            def prettyDump: String = {
                val targetStateStr = state.targetState match {
                    case TargetState.Uninitialized => "Uninitialized"
                    case TargetState.Active(treasuryUtxoId) =>
                        s"Active(treasuryUtxoId=${treasuryUtxoId})"
                    case TargetState.Finalized(finalizationTxHash) =>
                        s"Finalized(txHash=${finalizationTxHash})"
                }

                val entryPointsStr = state.happyPathSkeletonEntryPoints
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
                    .map { case (treasuryUtxo, fallbackTx) =>
                        val txHash = fallbackTx.tx.id
                        s"  ${treasuryUtxo} -> txHash=${txHash}"
                    }
                    .mkString("\n")

                s"""State(
                   |  targetState: ${targetStateStr}
                   |  happyPathSkeletonEntryPoints (${state.happyPathSkeletonEntryPoints.size} entries):
                   |${entryPointsStr}
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
    pendingConnections: HeadMultisigRegimeManager.PendingConnections | CardanoLiaison.Connections,
    tracer: ContraTracer[IO, CardanoLiaisonEvent],
    persistence: Persistence[IO],
    mrmSelf: ActorRef[IO, HeadMultisigRegimeManager.HandoffToRuleBased.type],
    advanceNodeStatus: NodeStatus => IO[Unit],
) extends Actor[IO, CardanoLiaison.Request]:
    import CardanoLiaison.*

    private val connections = Ref.unsafe[IO, Option[CardanoLiaison.Connections]](None)

    private val stateRef = Ref.unsafe[IO, CardanoLiaison.State](State.empty)

    private def getConnections: IO[Connections] = this.connections.get.flatMap(
      _.fold(
        IO.raiseError(
          RuntimeException("Consensus Actor is missing its connections to other actors.")
        )
      )(IO.pure)
    )

    private def initializeConnections: IO[Unit] = pendingConnections match {
        case x: HeadMultisigRegimeManager.PendingConnections =>
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
            // R3: rebuild the submission index by folding the persisted HardConfirmation CF (an
            // empty CF folds to `State.empty`). Submission progress itself is NOT persisted —
            // `runEffects` re-samples L1, so recover restores only the effect index.
            recovered <- State.recover(persistence)(using config)
            _ <- stateRef.set(recovered)
            _ <- advanceNodeStatus(nodeStatusOf(recovered.targetState))
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
      * Before stack 0, `State` is the cold [[State.empty]] — an `Uninitialized` target with no
      * effects, so nothing is submittable. Once stack 0 is hard-confirmed,
      * [[StackEffects.HardConfirmed.Initial]] carries the slow-consensus-ratified init tx (the
      * round-2 Initial unlock) + the locally-derived fallback; [[State.applyInitialEffects]]
      * instals them — the `Active` target, `EffectId.initializationEffectId`, and the initial
      * fallback keyed by the treasury utxo it spends — so `runEffects` submits the correct init tx.
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
            newState <- stateRef.updateAndGet(State.applyInitialEffects(_, eff))
            _ <- advanceNodeStatus(nodeStatusOf(newState.targetState))
            _ <- tracer.traceWith(CardanoLiaisonEvent.InitialStackEffectsState(newState.prettyDump))
        } yield ()

    /** Learn a hard-confirmed stack's L1 effects into the submission state machine.
      *
      * Translates [[StackEffects.HardConfirmed.Regular]] into the
      * `(happyPathSkeletonEntryPoints, happyPathEffects, fallbackEffects, targetState)` shape the
      * `runEffects` / `mkDirectActions` machinery consumes, so effects submit in dependency order
      * (backbone first via `EffectId (major, 0)`, then its rollouts `(major, 1..n)`; the fallback
      * resolved by the treasury utxo it spends — see below).
      *
      * Backbones (settlements, then the optional finalization) are walked in stack/major order;
      * each contributes one `EffectId` family keyed by its `majorVersionProduced`. Fallbacks are
      * keyed by the treasury utxo they spend (`treasurySpent.utxoId`) — the same utxo the NEXT
      * settlement consumes — so `mkDirectActions` finds a treasury's happy continuation and its
      * fallback under one key.
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
            // Counts for the trace event only — the state transition itself is
            // `State.applyRegularEffects`, the pure kernel shared with `State.recover`. Each
            // Major partition carries its settlement+fallback pair, so fallbacks == settlements.
            val settlements = parts.count {
                case _: PartitionEffects.Major[?] => true
                case _                            => false
            }
            val fallbacks = settlements
            val rollouts = parts.map {
                case PartitionEffects.Major(_, _, ro, _, _) => ro.size
                case PartitionEffects.Final(_, ro)          => ro.size
                case PartitionEffects.Minor(_, _)           => 0
            }.sum
            val hasFinalization = parts.exists {
                case _: PartitionEffects.Final => true
                case _                         => false
            }
            for {
                _ <- tracer.traceWith(
                  CardanoLiaisonEvent.StackEffectsLearned(
                    settlements,
                    fallbacks,
                    rollouts,
                    hasFinalization
                  )
                )
                // Single-threaded actor: get/compute/set is safe (no concurrent writer). A
                // disjoint-window violation is traced and re-raised — the liaison stops rather than
                // operating a head whose on-chain safety invariant is broken.
                oldState <- stateRef.get
                newState <- IO(State.applyRegularEffects(oldState, eff)).onError {
                    case v: DisjointWindowViolation =>
                        tracer.traceWith(
                          CardanoLiaisonEvent.DisjointWindowViolation(
                            v.treasuryUtxo.toString,
                            v.happyPathTtl.toString,
                            v.fallbackValidityStart.toString
                          )
                        )
                }
                _ <- stateRef.set(newState)
                _ <- advanceNodeStatus(nodeStatusOf(newState.targetState))
                _ <- tracer.traceWith(CardanoLiaisonEvent.StackEffectsState(newState.prettyDump))
            } yield ()
        }
    }

    /** The node lifecycle status implied by an L1 target state; reported through
      * `advanceNodeStatus` at every [[stateRef]] write.
      */
    private def nodeStatusOf(target: TargetState): NodeStatus = target match
        case TargetState.Uninitialized => NodeStatus.Initializing
        case TargetState.Active(_)     => NodeStatus.Active
        case TargetState.Finalized(_)  => NodeStatus.Finalized

    /** The core part of the liaison that decides whether an action is needed and submits them.
      *
      * It's called either when:
      *   - the liaison learns a new effect
      *   - by receiving timeout
      */
    private def runEffects: IO[Unit] = for {
        _ <- tracer.traceWith(CardanoLiaisonEvent.RunEffectsStarted)
        // 1. Get the L1 state, i.e. the list of utxo ids at the multisig address  + the current time
        resp <- cardanoBackend.utxosAt(config.headMultisigAddress)

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

                    // 3. Decide what to submit this cycle. See the decision schema in the class
                    //    doc: (1) due direct actions (incl. the tip fallback, resolved by utxo in
                    //    `mkDirectActions`); otherwise reconcile the target — (2) finalized, (3)
                    //    rule-based treasury present → handoff, (4) resubmit the skeleton.
                    actionsToSubmit <-
                        // (1) Effect inputs are on L1 — submit exactly those direct actions.
                        if dueActions.nonEmpty
                        then IO.pure(dueActions)
                        else
                            // Steps (2)/(3)/(4): nothing is due at the multisig address.
                            tracer.traceWith(CardanoLiaisonEvent.NoDirectActions) >>
                                reconcileTargetState(state, utxoIds, currentTime)

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
                            IO.traverse(actionsToSubmit.flatMap(actionTxs).toList)(etx =>
                                for {
                                    _ <- tracer.traceWith(
                                      CardanoLiaisonEvent.TxSubmitting(etx.tx.id)
                                    )
                                    ret <- cardanoBackend.submitTx(etx)
                                } yield (etx, ret)
                            )
                        else IO.pure(List.empty)

                    // Submission errors are ignored, but dumped here
                    submissionErrors = submitRet.filter(_._2.isLeft)
                    _ <- IO.whenA(submissionErrors.nonEmpty)(
                      tracer.traceWith(CardanoLiaisonEvent.SubmissionErrors(submissionErrors.size))
                    )

                    // The handoff to the rule-based regime is driven by the rule-based treasury
                    // probe in `reconcileTargetState` (step 3), not by an isTxKnown sweep over every
                    // known fallback tx here.

                } yield ()
        }
    } yield ()

    /** Steps (2)–(4) of the decision schema, reached when no L1 effect is due at the multisig
      * address this pass. Walks the target state:
      *   - `Uninitialized`: pre stack-0 — nothing submittable.
      *   - `Active` with its treasury utxo present, or `Finalized` with its finalization tx on L1:
      *     the head is in sync — nothing to do.
      *   - otherwise the target anchor is gone: probe the rule-based treasury and either hand off
      *     (step 3) or resubmit the skeleton (step 4), via [[handoffOrResubmit]].
      */
    private def reconcileTargetState(
        state: State,
        utxoIds: Set[TransactionInput],
        currentTime: QuantizedInstant
    ): IO[Seq[Action]] =
        state.targetState match {
            case TargetState.Uninitialized =>
                // Pre stack-0: no L1 target to reconcile and nothing submittable — wait for the
                // Initial stack effects.
                IO.pure(Seq.empty)

            case TargetState.Active(targetTreasuryUtxoId) =>
                if utxoIds.contains(targetTreasuryUtxoId) then
                    // (1) Present — L1 is in sync with the target; anything due was already a direct
                    // action above.
                    tracer.traceWith(
                      CardanoLiaisonEvent
                          .TargetUtxoStatus(targetTreasuryUtxoId.toString, found = true)
                    ) >> IO.pure(Seq.empty)
                else
                    // The treasury is gone: either we fell into the rule-based regime, or an L1
                    // rollback took it. Steps (3)/(4) tell those apart.
                    tracer.traceWith(
                      CardanoLiaisonEvent
                          .TargetUtxoStatus(targetTreasuryUtxoId.toString, found = false)
                    ) >> handoffOrResubmit(state, currentTime)

            case TargetState.Finalized(finalizationTxHash) =>
                // (2) Finalization is the target: if its tx is on L1 the head is settled for good.
                cardanoBackend.isTxKnown(finalizationTxHash).flatMap {
                    // Couldn't query its status — skip this cycle.
                    case Left(err) =>
                        tracer.traceWith(
                          CardanoLiaisonEvent.FinalizationTxQueryError(err.toString)
                        ) >> IO.pure(Seq.empty)
                    case Right(true) =>
                        tracer.traceWith(
                          CardanoLiaisonEvent
                              .FinalizationTxStatus(finalizationTxHash.toString, "known")
                        ) >> IO.pure(Seq.empty)
                    // Not on L1 — possible rollback of the finalization; fall to steps (3)/(4).
                    case Right(false) =>
                        tracer.traceWith(
                          CardanoLiaisonEvent
                              .FinalizationTxStatus(finalizationTxHash.toString, "not known")
                        ) >> handoffOrResubmit(state, currentTime)
                }
        }

    /** Steps (3) then (4): the target anchor (treasury utxo / finalization tx) is not on L1. First
      * probe the rule-based treasury beacon — if it exists the head has fallen into the rule-based
      * regime, so hand off (idempotent) and submit nothing. Otherwise it is a genuine L1 rollback:
      * resubmit the skeleton via [[resubmitSkeleton]].
      */
    private def handoffOrResubmit(state: State, currentTime: QuantizedInstant): IO[Seq[Action]] =
        ruleBasedTreasuryPresent.flatMap {
            // Couldn't probe — skip this cycle, retry on the next tick.
            case Left(err) =>
                tracer.traceWith(CardanoLiaisonEvent.RuleBasedTreasuryQueryError(err.toString)) >>
                    IO.pure(Seq.empty)
            // (3) The rule-based treasury is on L1 — hand off. Every peer that observes it hands
            // off, not just the fallback's submitter; the MRM handler is idempotent so re-fires are
            // safe. The rule-based side reads its version/status from this treasury utxo, so the
            // trigger carries no tx. `txId` is the tx that produced the observed treasury utxo.
            case Right(Some(treasuryUtxoId)) =>
                tracer.traceWith(
                  CardanoLiaisonEvent.FallbackToRuleBasedDispatched(treasuryUtxoId.transactionId)
                ) >>
                    (mrmSelf ! HeadMultisigRegimeManager.HandoffToRuleBased) >>
                    IO.pure(Seq.empty)
            // (4) No rule-based treasury — a real rollback.
            case Right(None) =>
                resubmitSkeleton(state, currentTime)
        }

    /** Step (4): the target anchor is gone and there is no rule-based treasury — a genuine L1
      * rollback. If the init tx is no longer on L1 the head must be re-established from scratch, so
      * resubmit the whole skeleton (`InitializeHead`) — but only while inside its validity window
      * (`currentTime < initializationTxEndTime`); once that passes the init tx can never confirm
      * and the head cannot be re-established (`InitWindowElapsed`). If the init tx still stands the
      * head is up; nothing is resubmitted here — step (1)'s direct actions push it forward as
      * inputs reappear. The init tx (and its window) come from the hard-confirmed stack 0 held in
      * `happyPathEffects`, not the unsigned config body.
      */
    private def resubmitSkeleton(state: State, currentTime: QuantizedInstant): IO[Seq[Action]] =
        state.happyPathEffects.get(EffectId.initializationEffectId) match {
            case Some(initTx: InitializationTx) =>
                cardanoBackend.isTxKnown(initTx.tx.id).flatMap {
                    case Left(err) =>
                        tracer.traceWith(CardanoLiaisonEvent.InitTxQueryError(err.toString)) >>
                            IO.pure(Seq.empty)
                    case Right(true) =>
                        // Head is established — leave recovery to the direct-action path.
                        tracer.traceWith(
                          CardanoLiaisonEvent.InitTxStatus(initTx.tx.id.toString, "known")
                        ) >> IO.pure(Seq.empty)
                    case Right(false) =>
                        val initEnd = initTx.initializationTxEndTime.convert
                        if currentTime < initEnd then
                            tracer.traceWith(
                              CardanoLiaisonEvent.InitTxStatus(initTx.tx.id.toString, "not known")
                            ) >> IO.pure(
                              Seq(Action.InitializeHead(state.happyPathEffects.values.toSeq))
                            )
                        else
                            tracer.traceWith(
                              CardanoLiaisonEvent
                                  .InitWindowElapsed(currentTime.toString, initEnd.toString)
                            ) >> IO.pure(Seq.empty)
                }
            case _ =>
                // No hard-confirmed init tx in state yet — nothing to resubmit.
                IO.pure(Seq.empty)
        }

    /** Probe L1 for the rule-based treasury: the treasury-token beacon at the rule-based treasury
      * script address. Its presence means the head has fallen into the rule-based regime. Returns
      * the treasury utxo's id when found (its `transactionId` names the producing tx, for tracing).
      */
    private def ruleBasedTreasuryPresent
        : IO[Either[CardanoBackend.Error, Option[TransactionInput]]] =
        cardanoBackend
            .utxosAt(
              HydrozoaBlueprint.mkTreasuryAddress(config.network),
              (config.headMultisigScript.policyId, config.headTokenNames.treasuryTokenName)
            )
            .map(_.map(_.keySet.headOption))

    // ===================================
    // Actions
    // ===================================

    /** The set of effects the actor may want to execute against L1. */
    sealed trait Action
    sealed trait DirectAction extends Action

    // TODO: narrow these element types beyond `EnrichedTx[?]` — the construction sites already
    // carry concrete subtypes:
    //   - FallbackToRuleBased.tx        => FallbackTx
    //   - PushForwardMultisig.txs       => Seq[SettlementTx | FinalizationTx | RolloutTx]
    //   - Rollout.txs                   => Seq[RolloutTx]
    //   - InitializeHead.txs            => Seq[HappyPathEffect]
    // Tightening these makes the `.tx` strip at the construction sites a compile error rather
    // than a code-review catch (the "throwing away useful information too early" pattern that
    // motivated this refactor). HappyPathEffect would need to be promoted out of the trait body.
    object Action {

        /** Switching into the rule-based regime. */
        final case class FallbackToRuleBased(tx: FallbackTx) extends DirectAction

        /** Pushing the existing state in the multisig regime forward. */
        final case class PushForwardMultisig(txs: Seq[EnrichedTx[?]]) extends DirectAction

        /** Finalizing a rollout sequence. */
        final case class Rollout(txs: Seq[EnrichedTx[?]]) extends DirectAction

        /** Represents noop action that may occur when the current time falls into the silence
          * period - the gap between a treasury's two disjoint validity windows, when the
          * settlement/finalization tx already expired but the fallback is not valid yet.
          */
        final case class SilencePeriodNoop(
            currentTime: QuantizedInstant,
            happyPathTxTtl: QuantizedInstant,
            fallbackValidityStart: FallbackTxStartTime
        ) extends DirectAction {}

        /** Like [[PushForwardMultisig]] but starting from the initialization tx. */
        final case class InitializeHead(txs: Seq[EnrichedTx[?]]) extends Action
    }

    private def actionTxs(action: Action): Seq[EnrichedTx[?]] = action match {
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
                case FallbackToRuleBased(tx)         => s"FallbackToRuleBased (${tx.tx.id})"
                case PushForwardMultisig(txs)        => s"PushForwardMultisig (${txs.map(_.tx.id)}"
                case Rollout(txs)                    => s"Rollout (${txs.map(_.tx.id)}"
                case sp @ SilencePeriodNoop(_, _, _) => s"$sp"
                case InitializeHead(txs)             => s"InitializeHead (${txs.map(_.tx.id)}"
            }

    /** A polled utxo classified as a due direct-action trigger, so [[mkDirectAction]] is total over
      * well-formed inputs. [[triggerFor]] owns the malformed combinations: a utxo that is neither
      * an entry point nor a fallback is dropped, and a backbone missing its fallback (or its
      * settlement/finalization body) surfaces an [[EffectError]] there.
      */
    private enum DirectTrigger:
        /** A rollout continuation — entry point `(major, index)` with `index != 0`. */
        case Rollout(rolloutId: EffectId)

        /** A settlement/finalization treasury — entry point `(major, 0)` — with its fallback. */
        case Backbone(
            backboneId: EffectId,
            backboneTx: SettlementTx | FinalizationTx,
            fallback: FallbackTx
        )

        /** The last settlement's treasury: only a fallback, no next backbone learned yet. */
        case TipFallback(fallback: FallbackTx)

    private def mkDirectActions(
        state: State,
        utxosFound: Set[TransactionInput],
        currentTime: QuantizedInstant
    ): Either[EffectError, Seq[DirectAction]] =
        utxosFound.toSeq
            .flatMap(triggerFor(state, _)) // drops utxos that are none of ours
            .sequence
            // Entry points (a backbone before its rollouts) in EffectId order; a tip fallback (no
            // entry point) has no ordering constraint and comes last.
            .map(_.sortBy(triggerSortKey).flatMap(mkDirectAction(state, currentTime)))

    /** Classify one polled utxo. `None` — the utxo is neither an entry point nor a fallback (not
      * ours). `Some(Left(_))` — a malformed backbone (its fallback or its settlement/finalization
      * body is missing). `Some(Right(_))` — a well-formed [[DirectTrigger]].
      */
    private def triggerFor(
        state: State,
        utxo: TransactionInput
    ): Option[Either[EffectError, DirectTrigger]] = {
        import EffectError.*
        (state.happyPathSkeletonEntryPoints.get(utxo), state.fallbackEffects.get(utxo)) match {
            case (None, None) => None
            case (Some(rolloutId @ (_, index)), _) if index != 0 =>
                Some(Right(DirectTrigger.Rollout(rolloutId)))
            case (Some(backboneId), mbFallback) =>
                Some(for {
                    fallback <- mbFallback.toRight(FallbackNotFound(backboneId))
                    backboneTx <- state.happyPathEffects(backboneId) match {
                        case tx: SettlementTx    => Right(tx)
                        case tx: FinalizationTx  => Right(tx)
                        case _: InitializationTx => Left(UnexpectedInitializationEffect(backboneId))
                        case _: RolloutTx        => Left(UnexpectedRolloutEffect(backboneId))
                    }
                } yield DirectTrigger.Backbone(backboneId, backboneTx, fallback))
            case (None, Some(fallback)) =>
                Some(Right(DirectTrigger.TipFallback(fallback)))
        }
    }

    private def triggerSortKey(trigger: DirectTrigger): (Boolean, Option[EffectId]) =
        trigger match {
            case DirectTrigger.Rollout(rolloutId) => (false, Some(rolloutId))
            case DirectTrigger.Backbone(id, _, _) => (false, Some(id))
            case DirectTrigger.TipFallback(_)     => (true, None)
        }

    /** Decide the action for one classified [[DirectTrigger]]. Total: a treasury's happy tx and its
      * fallback have disjoint validity windows separated by a silence period, so at most one is
      * submittable; `None` means nothing is submittable now (the silence period, or a tip fallback
      * whose window has not opened yet).
      */
    private def mkDirectAction(state: State, currentTime: QuantizedInstant)(
        trigger: DirectTrigger
    ): Option[DirectAction] = {
        import Action.*
        trigger match {
            // Rollout continuation — the rollout chain from here up to the next backbone.
            case DirectTrigger.Rollout(rolloutId @ (versionMajor, _)) =>
                val nextBackbone = versionMajor.increment -> 0
                Some(Rollout(state.happyPathEffects.range(rolloutId, nextBackbone).toSeq.map(_._2)))

            // Submit the happy tx while its window is open, else the fallback once its (later,
            // disjoint) window opens, else nothing during the silence period between them.
            case DirectTrigger.Backbone(backboneId, backboneTx, fallback) =>
                val happyTtl: QuantizedInstant = backboneTx match {
                    case tx: SettlementTx   => tx.settlementTxEndTime.convert
                    case tx: FinalizationTx => tx.finalizationTxEndTime.convert
                }
                val fallbackStart = fallback.fallbackTxStartTime
                if currentTime < happyTtl then
                    Some(
                      PushForwardMultisig(
                        state.happyPathEffects.rangeFrom(backboneId).toSeq.map(_._2)
                      )
                    )
                else if currentTime >= fallbackStart then Some(FallbackToRuleBased(fallback))
                else Some(SilencePeriodNoop(currentTime, happyTtl, fallbackStart))

            // A tip fallback: submit it once its window opens.
            case DirectTrigger.TipFallback(fallback) =>
                Option.when(fallback.fallbackTxStartTime.convert <= currentTime)(
                  FallbackToRuleBased(fallback)
                )
        }
    }

    private enum EffectError extends Throwable:
        case UnexpectedRolloutEffect(effectId: EffectId)
        case UnexpectedInitializationEffect(effectId: EffectId)
        case FallbackNotFound(effectId: EffectId)

    import EffectError.*

    extension (self: EffectError)
        private def msg: String = self match {
            case UnexpectedRolloutEffect(effectId) =>
                s"Unexpected rollout effect with effectId = $effectId, check the integrity of effects."
            case UnexpectedInitializationEffect(effectId) =>
                s"Unexpected initialization effect with effectId = $effectId, check the integrity of effects and the initialization tx."
            case FallbackNotFound(effectId) =>
                s"Impossible: a settlement/finalization effect ($effectId) without a fallback tx."
        }

end CardanoLiaison
