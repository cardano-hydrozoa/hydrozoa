package hydrozoa.multisig.consensus

import cats.effect.{IO, Ref}
import cats.implicits.*
import com.suprnation.actor.Actor.{Actor, Receive}
import hydrozoa.UtxoIdL1
import hydrozoa.multisig.consensus.CardanoLiaison.{Config, ConnectionsPending}
import hydrozoa.multisig.ledger.dapp.tx.*
import hydrozoa.multisig.protocol.CardanoBackendProtocol.CardanoBackend
import hydrozoa.multisig.protocol.CardanoBackendProtocol.CardanoBackend.{GetCardanoHeadStateResp, SubmitL1Effects}
import hydrozoa.multisig.protocol.ConsensusProtocol.*
import hydrozoa.multisig.protocol.ConsensusProtocol.CardanoLiaison.*
import hydrozoa.multisig.protocol.PersistenceProtocol.*
import hydrozoa.multisig.protocol.types.Block
import scala.collection.mutable
import scalus.cardano.ledger.{Slot, Transaction, TransactionHash}

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
    final case class Config(
        cardanoBackend: CardanoBackend.Ref,
        persistence: Persistence.Ref,
        initializationTx: InitializationTx,
        initializationFallbackTx: FallbackTx
    )

    final case class ConnectionsPending()

    def apply(config: Config, connections: ConnectionsPending): IO[CardanoLiaison] = {
        IO(new CardanoLiaison(config, connections) {})
    }
}

trait CardanoLiaison(config: Config, _connections: ConnectionsPending) extends Actor[IO, Request] {
    private val subscribers = Ref.unsafe[IO, Option[Subscribers]](None)
    private val state: State = ???
    // State(
    //  Map.empty,
    //  Map.empty,
    //  mutable.Map(
    //    UtxoIdL1(
    //      config.initializationFallbackTx.treasurySpent.utxoId
    //    ) -> config.initializationFallbackTx
    //  ),
    //  Ref.unsafe[IO, Option[FinalizationTxSeq]](None)
    // )

    private final case class Subscribers()

    override def preStart: IO[Unit] =
        for {
            _ <- subscribers.set(
              Some(
                Subscribers(
                )
              )
            )
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

    private def receiveTotal(req: Request, subs: Subscribers): IO[Unit] =
        req match {
            case _: ConfirmBlock => ???
            case effects: MajorBlockL1Effects =>
                handleMajorBlockL1Effects(effects)
            case effects: FinalBlockL1Effects =>
                handleFinalBlockL1Effects(effects)
        }

    // ===================================
    // New effects handlers
    // ===================================

    /** Handle [[MajorBlockL1Effects]] request:
      *   - saves the effects in the internal actor's state
      */
    private def handleMajorBlockL1Effects(effects: MajorBlockL1Effects) = ???

    /** Handle [[FinalBlockL1Effects]] request:
      *   - saves the effects in the internal actor's state
      */
    private def handleFinalBlockL1Effects(effects: FinalBlockL1Effects) = ???

    // ===================================
    // Internal state
    // ===================================

    /** The second part of the EffectId is a number:
      *   - 0 - settlement
      *   - 1,2,3,... - rollouts
      */
    private type EffectId = (Block.Number, Int)

    /** The existence of this trait is due to the fact the [[Tx]] trait is not sealed. */
    private sealed trait EffectTx:
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

        /** Final state of a head, represented by the transaction hash of the finalization tx.
          *
          * TODO: Verify: We need it to be able to check whether the finalization happened so we can
          * decide how to act when the treasury is absent.
          */
        case Finalized(finalizationTxHash: TransactionHash)

    /** Internal state of the actor. */
    private final case class State(
        effectInputs: mutable.Map[UtxoIdL1, EffectId],
        effects: mutable.TreeMap[EffectId, EffectTx],
        /** Fallback txs, indexed by block number they belong to. */
        fallbackTxs: mutable.Map[Block.Number, FallbackTx],
        targetState: Ref[IO, TargetState]
    )

    // ===================================
    // Main loop
    // ===================================

    private enum Action:
        case FallbackToRuleBased(tx: Transaction)
        case PushForwardMultisig(txs: Seq[Transaction])
        case Rollout(txs: Seq[Transaction])
        case InitializeHead(txs: Seq[Transaction])

    private def actionTxs(action: Action): Seq[Transaction] = action match {
        case Action.FallbackToRuleBased(tx)  => Seq(tx)
        case Action.PushForwardMultisig(txs) => txs
        case Action.Rollout(txs)             => txs
        case Action.InitializeHead(txs)      => txs
    }

    /** TODO: this should be called periodically somehow, or an external timer should send us a
      * message via the actor's inbox.
      */
    private def loop(): Unit = {

        // 1. Get the L1 state, i.e. the list of utxo ids + the current slot
        // TODO: call config.cardanoBackend ?: GetCardanoHeadState()
        val resp: GetCardanoHeadStateResp = ???

        // 2. Based on the local state, find all direct actions
        // (i.e. those that are directly caused by effect inputs in L1 response).
        val directActions: Seq[Action] = mkDirectActions(resp.utxoIds, resp.currentSlot)

        // 3. Determine whether the initialization requires submission.
        val actionsToSubmit =
            if directActions.nonEmpty
            then directActions
            else
                // Empty direct actions is a precondition for the init tx submission
                val initAction = Seq(Action.InitializeHead(state.effects.values.map(_.tx).toSeq))
                // FIXME: run properly
                val targetState: TargetState = ??? // state.targetState.get.unsafeRunAsync(???)(???)
                targetState match {
                    case TargetState.Active(targetTreasuryUtxoId) =>
                        if resp.utxoIds.contains(targetTreasuryUtxoId) then List.empty
                        else initAction
                    case TargetState.Finalized(finalizationTxHash) =>
                        // TODO: call config.cardanoBackend ?: GetTxInfo(finalizationTxHash)
                        val wasSubmitted: Boolean = ???
                        if wasSubmitted then Seq.empty else initAction
                }

        // 4. Submit flattened txs for actions
        submitEffects(actionsToSubmit.map(actionTxs).flatten.toList)
    }

    private def submitEffects(txs: List[Transaction]): Unit = {
        val ret = config.cardanoBackend ! SubmitL1Effects(txs)
        // TODO: run properly
        ret.unsafeRunAsync(_ => ())(using ???)
    }

    private def mkDirectActions(utxosFound: Seq[UtxoIdL1], slot: Slot): Seq[Action] =
        // Split into known effect inputs/unknown utxos
        // val (inputs, _unknown) = utxosFound.partition(state.effectInputs.keySet.contains)

        utxosFound
            .map(state.effectInputs.get)
            .filter(_.isDefined)
            .map(_.get)
            .map(mkDirectAction(slot))

    private def mkDirectAction(currentSlot: Slot)(
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
                if mbFallbackTx.isDefined && mbFallbackTx.get.validityStartSlot >= currentSlot then
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
    // Errors
    // ===================================

    enum Errors:
        /** The state already contains a fallback tx for a particular utxo. */
        case NonUniqueFallbackTx()
}
