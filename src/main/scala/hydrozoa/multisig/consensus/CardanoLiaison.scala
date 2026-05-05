package hydrozoa.multisig.consensus

import cats.effect.{IO, IOLocal, Ref}
import cats.implicits.*
import com.bloxbean.cardano.client.util.HexUtil
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.ActorRef
import hydrozoa.config.head.initialization.InitialBlock
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.FallbackTxStartTime
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.node.operation.multisig.NodeOperationMultisigConfig
import hydrozoa.lib.cardano.scalus.QuantizedTime.{QuantizedInstant, toEpochQuantizedInstant}
import hydrozoa.lib.logging.Tracer
import hydrozoa.multisig.MultisigRegimeManager
import hydrozoa.multisig.backend.cardano.CardanoBackend
import hydrozoa.multisig.consensus.pollresults.PollResults
import hydrozoa.multisig.ledger.block.BlockVersion.Major.increment
import hydrozoa.multisig.ledger.block.{BlockEffects, BlockHeader, BlockVersion}
import hydrozoa.multisig.ledger.l1.tx.*
import scala.collection.immutable.{Seq, TreeMap}
import scala.math.Ordered.orderingToOrdered
import scalus.cardano.ledger.Transaction.given
import scalus.cardano.ledger.{Block as _, BlockHeader as _, Transaction, TransactionHash, TransactionInput}
import scalus.utils.Pretty

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
        tracerLocal: IOLocal[Tracer]
    ): IO[CardanoLiaison] =
        IO(new CardanoLiaison(config, cardanoBackend, pendingConnections, tracerLocal) {})

    type Config = CardanoNetwork.Section & InitialBlock.Section &
        NodeOperationMultisigConfig.Section

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
            State(
              targetState = TargetState.Active(config.initializationTx.treasuryProduced.utxoId),
              effectInputs = Map.empty,
              happyPathEffects =
                  TreeMap(EffectId.initializationEffectId -> config.initializationTx),
              fallbackEffects = Map(BlockVersion.Major.zero -> config.initialFallbackTx)
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

    object BlockConfirmed {
        type Major = BlockHeader.Fields.HasBlockNum & BlockHeader.Fields.HasBlockVersion &
            BlockEffects.MultiSigned.Major.Section
        type Final = BlockHeader.Fields.HasBlockNum & BlockHeader.Fields.HasBlockVersion &
            BlockEffects.MultiSigned.Final.Section

        /** For testing purposes, where we may not want to construct a whole Block.MultiSigned. */
        sealed trait Minimal extends BlockHeader.Fields.HasBlockVersion

        object Minimal {

            /** For testing purposes, where we may not want to construct a whole Block.MultiSigned.
              */
            final case class Major(
                override val blockVersion: BlockVersion.Full,
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
                override val blockVersion: BlockVersion.Full,
                override val finalizationTx: FinalizationTx,
                override val rolloutTxs: List[RolloutTx],
            ) extends Minimal,
                  BlockEffects.MultiSigned.Final.Section {
                override def effects: BlockEffects.MultiSigned.Final =
                    BlockEffects.MultiSigned.Final(finalizationTx, rolloutTxs)
            }
        }

    }

    type Request = PreStart.type | BlockConfirmed.Major | BlockConfirmed.Final | Timeout.type
    type Handle = ActorRef[IO, Request]

    case object PreStart

end CardanoLiaison

trait CardanoLiaison(
    config: CardanoLiaison.Config,
    cardanoBackend: CardanoBackend[IO],
    pendingConnections: MultisigRegimeManager.PendingConnections | CardanoLiaison.Connections,
    tracerLocal: IOLocal[Tracer],
) extends Actor[IO, CardanoLiaison.Request]:
    import CardanoLiaison.*

    given IOLocal[Tracer] = tracerLocal

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
        Tracer.scoped(_.copy(logger = "CardanoLiaison")) {
            req match {
                case CardanoLiaison.PreStart =>
                    preStartLocal
                case block: BlockConfirmed.Major =>
                    Tracer.scopedCtx(
                      "cardanoLiaisonMode" -> "BlockConfirmed.Major",
                      "blockNum" -> s"${block.blockNum: Int}"
                    ) {
                        Tracer.info(
                          s"received BlockConfirmed.Major for block ${block.blockVersion}"
                        ) >>
                            handleMajorBlockL1Effects(block) >> runEffects
                    }
                case block: BlockConfirmed.Final =>
                    Tracer.scopedCtx(
                      "cardanoLiaisonMode" -> "BlockConfirmed.Final",
                      "blockNum" -> s"${block.blockNum: Int}"
                    ) {
                        Tracer.info(
                          s"received BlockConfirmed.Final for block ${block.blockVersion}"
                        ) >>
                            handleFinalBlockL1Effects(block) >> runEffects
                    }
                case CardanoLiaison.Timeout =>
                    Tracer.scopedCtx("cardanoLiaisonMode" -> "Timeout") {
                        Tracer.info("received Timeout, run effects...") >>
                            runEffects
                    }
            }
        }

    private def preStartLocal: IO[Unit] =
        for {
            _ <- Tracer.updateLocal(_.copy(logger = "CardanoLiaison"))
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

    /** Handle [[Block.MultiSigned.Major]] request:
      *   - saves the effects in the internal actor's state
      */
    protected[consensus] def handleMajorBlockL1Effects(block: BlockConfirmed.Major): IO[Unit] =
        for {
            _ <- Tracer.info(s"entering handleMajorBlockL1Effects for block ${block.blockVersion}")
            _ <- IO.whenA(block.blockVersion.major != block.settlementTx.majorVersionProduced) {
                val msg =
                    s"Block major version (${block.blockVersion.major}) doesn't match" +
                        s" settlement tx major version produced (${block.settlementTx.majorVersionProduced})"
                Tracer.error(msg) >> IO.raiseError(RuntimeException(msg))
            }

            _ <- Tracer.trace(s"settlementTx hash: ${block.settlementTx.tx.id}")
            _ <- Tracer.trace(s"settlementTx ttl: ${block.settlementTx.tx.body.value.ttl}")
            _ <- Tracer.trace(
              "settlementTx treasuryProduced: " +
                  s"${block.settlementTx.treasuryProduced.utxoId}"
            )
            _ <- Tracer.trace(
              "settlementTx majorVersionProduced: " +
                  s"${block.settlementTx.majorVersionProduced}"
            )
            _ <- Tracer.trace(
              s"fallback tx validity start: ${block.fallbackTx.fallbackTxStartTime}"
            )

            newState <- stateRef.updateAndGet(s => {
                val (blockEffectInputs, blockEffects) =
                    mkHappyPathEffectInputsAndEffects(block.settlementTx, block.rolloutTxs)
                State(
                  targetState = TargetState.Active(
                    block.settlementTx.treasuryProduced.utxoId
                  ),
                  effectInputs = s.effectInputs ++ blockEffectInputs,
                  happyPathEffects = s.happyPathEffects ++ blockEffects,
                  fallbackEffects =
                      s.fallbackEffects + (block.blockVersion.major -> block.fallbackTx)
                )
            })
            _ <- Tracer.trace(s"state after update: ${newState.prettyDump}")
        } yield ()

    /** Handle [[Block.MultiSigned.Final]] request:
      *   - saves the effects in the internal actor's state
      */
    protected[consensus] def handleFinalBlockL1Effects(block: BlockConfirmed.Final): IO[Unit] =
        for {
            _ <- Tracer.info(s"entering handleFinalBlockL1Effects for block ${block.blockVersion}")

            _ <- Tracer.trace(s"finalizationTx hash: ${block.finalizationTx.tx.id}")
            _ <- Tracer.trace(s"rolloutTxs count: ${block.rolloutTxs.size}")

            newState <- stateRef.updateAndGet(s => {
                val (blockEffectInputs, blockEffects) =
                    mkHappyPathEffectInputsAndEffects(
                      block.finalizationTx,
                      block.rolloutTxs
                    )
                s.copy(
                  targetState = TargetState.Finalized(block.finalizationTx.tx.id),
                  effectInputs = s.effectInputs ++ blockEffectInputs,
                  happyPathEffects = s.happyPathEffects ++ blockEffects
                )
            })
            _ <- Tracer.trace(s"state after update: ${newState.prettyDump}")
        } yield ()

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
        _ <- Tracer.trace("entering `runEffects`")
        // 1. Get the L1 state, i.e. the list of utxo ids at the multisig address  + the current time
        resp <- cardanoBackend.utxosAt(config.initializationTx.treasuryProduced.address)

        _ <- resp match {

            case Left(err) =>
                // This may happen if L1 API is temporarily unavailable or misconfigured
                // TODO: we need to address time when we work on autonomous mode
                //   but for now we can just ignore it and skip till the next event/timeout
                Tracer.error(s"error when getting Cardano L1 state: $err")

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

                    _ <- Tracer.trace(s"current time is $currentTime")
                    _ <- Tracer.trace(s"utxoIds are $utxoIds")
                    _ <- Tracer.trace(s"state is ${state.prettyDump}")

                    // (i.e. those that are directly caused by effect inputs in L1 response).
                    dueActions: Seq[DirectAction] <- mkDirectActions(
                      state,
                      utxoIds,
                      currentTime
                    ).fold(
                      e =>
                          Tracer.error(s"Critical error: ${e.msg}") >>
                              IO.raiseError(RuntimeException(e.msg)),
                      IO.pure
                    )
                    // .fold(e => {throw RuntimeException(e.msg)}, x => x)

                    actionsToSubmit <-
                        if dueActions.nonEmpty
                        then IO.pure(dueActions)
                        else {

                            Tracer.trace("due actions is empty")

                            // Empty direct actions indicate another actions should be considered:
                            //  - the last fallback tx might have become valid
                            //  - the init (whole happy path) tx submission might be needed

                            // TODO: this is done in a bit a makeshift manner to fix the test, likely we want to do it
                            //   more systematically
                            val lastFallback: Option[Transaction] = for {
                                maxKey <- state.fallbackEffects.keySet.maxOption
                                fallbackTx = state.fallbackEffects(maxKey)
                                if utxoIds.contains(
                                  fallbackTx.treasurySpent.utxoId
                                ) && fallbackTx.fallbackTxStartTime.convert <= currentTime
                            } yield fallbackTx.tx

                            lastFallback match {
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
                                            then {
                                                // everything is up-to-date on L1
                                                Tracer.trace(
                                                  s"target ${targetTreasuryUtxoId} found, do nothing"
                                                ) >>
                                                    IO.pure(List.empty)
                                            } else
                                                Tracer.trace(
                                                  s"no target ${targetTreasuryUtxoId} found, submitting initAction: ${initAction.headOption.map(_.txs.map(_.id))}"
                                                ) >>
                                                    IO.pure(initAction)

                                        case TargetState.Finalized(finalizationTxHash) =>
                                            for {
                                                txResp <- cardanoBackend.isTxKnown(
                                                  finalizationTxHash
                                                )
                                                _ <- Tracer.debug(
                                                  s"finalizationTx: hash: $finalizationTxHash txResp: $txResp"
                                                )
                                                mbInitAction <- txResp match {
                                                    case Left(err) =>
                                                        for {
                                                            _ <- Tracer.error(
                                                              s"error when getting finalization tx info: ${err}"
                                                            )
                                                        } yield Seq.empty
                                                    case Right(isKnown) =>
                                                        if isKnown
                                                        then
                                                            Tracer.trace(
                                                              "finalization tx is known, do nothing"
                                                            ) >> IO.pure(Seq.empty)
                                                        else
                                                            Tracer.trace(
                                                              s"finalization tx is NOT known, submitting initAction: ${initAction.headOption.map(_.txs.map(_.id))}"
                                                            ) >> IO.pure(initAction)
                                                }
                                            } yield mbInitAction
                                    }
                                }
                            }
                        }

                    // 4. Submit flattened txs for actions it there are some
                    _ <- IO.whenA(actionsToSubmit.nonEmpty) {
                        val hasFallback =
                            actionsToSubmit.exists(action =>
                                action.isInstanceOf[Action.FallbackToRuleBased] ||
                                    action.isInstanceOf[Action.SilencePeriodNoop]
                            )
                        val msg = "Liaison's actions:" + actionsToSubmit
                            .map(a => s"\n\t- ${a.msg}")
                            .mkString
                        if hasFallback
                        then Tracer.warn(msg)
                        else Tracer.info(msg)
                    }

                    submitRet <-
                        if actionsToSubmit.nonEmpty then
                            IO.traverse(actionsToSubmit.flatMap(actionTxs).toList)(tx =>
                                for {
                                    _ <- Tracer.trace(
                                      s"Submitting tx hash: ${tx.id}\n\t" +
                                          s"Pretty: ${summon[Pretty[Transaction]].pretty(tx)}" +
                                          s"\n\tcbor: ${HexUtil.encodeHexString(tx.toCbor)}"
                                    )
                                    ret <- cardanoBackend.submitTx(tx)
                                } yield tx -> ret
                            )
                        else IO.pure(List.empty)

                    // Submission errors are ignored, but dumped here
                    submissionErrors = submitRet.filter(_._2.isLeft)
                    _ <- IO.whenA(submissionErrors.nonEmpty)(
                      Tracer.debug(
                        "Submission errors (generally ignored):" + submissionErrors
                            .map(a =>
                                s"\n\t- ${a._2.left},\n\t " +
                                    s"Pretty: ${summon[Pretty[Transaction]].pretty(a._1)}\n\t " +
                                    s"cbor=${HexUtil.encodeHexString(a._1.toCbor)}"
                            )
                            .mkString
                      )
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
