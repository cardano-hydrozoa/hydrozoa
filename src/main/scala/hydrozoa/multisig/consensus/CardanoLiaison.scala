package hydrozoa.multisig.consensus

import cats.effect.{IO, Ref}
import cats.implicits.*
import com.suprnation.actor.Actor.{Actor, Receive}
import hydrozoa.UtxoIdL1
import hydrozoa.multisig.consensus.CardanoLiaison.{Config, ConnectionsPending}
import hydrozoa.multisig.ledger.dapp.tx.*
import hydrozoa.multisig.ledger.dapp.txseq.{FinalizationTxSeq, SettlementTxSeq}
import hydrozoa.multisig.protocol.CardanoBackendProtocol.CardanoBackend
import hydrozoa.multisig.protocol.ConsensusProtocol.*
import hydrozoa.multisig.protocol.ConsensusProtocol.CardanoLiaison.*
import hydrozoa.multisig.protocol.PersistenceProtocol.*
import hydrozoa.multisig.protocol.types.Block
import scala.collection.mutable
import scalus.cardano.ledger.Transaction

/** Hydrozoa's liaison to Cardano L1 (actor):
  *
  * TODO: this is kind of too bold: we can't * do that due to L1 rollbacks
  *   - Keeps track of confirmed L1 effects of L2 blocks.
  *
  * And:
  *
  *   - Periodically polls the Cardano blockchain for the head's utxo state.
  *   - Submits whichever L1 effects are not yet reflected in the Cardano blockchain.
  *
  * TODO: Some notes:
  *   - We can't die once we submit the fallback tx since there may be unfinished rollouts
  *   - More broadly, we don't want to die even there is nothing to submit - an L1 rollback may
  *     happen.
  */
object CardanoLiaison {
    final case class Config(
        cardanoBackend: CardanoBackend.Ref,
        persistence: Persistence.Ref,
        initializationTx: InitializationTx,
        fallbackTx: FallbackTx
    )

    final case class ConnectionsPending()

    def apply(config: Config, connections: ConnectionsPending): IO[CardanoLiaison] = {
        IO(new CardanoLiaison(config, connections) {})
    }
}

trait CardanoLiaison(config: Config, _connections: ConnectionsPending) extends Actor[IO, Request] {
    private val subscribers = Ref.unsafe[IO, Option[Subscribers]](None)
    State(
      Map.empty,
      Map.empty,
      mutable.Map(UtxoIdL1(config.fallbackTx.treasurySpent.utxoId) -> config.fallbackTx),
      Ref.unsafe[IO, Option[FinalizationTxSeq]](None)
    )

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

    // private final case class Effect(
    //    self: InitializationTx | SettlementTxSeq | FinalizationTxSeq,
    //    link: Option[Effect]
    // )

    private final case class State(
        effects: Map[Block.Number, (SettlementTxSeq, FallbackTx)],
        txs: Map[UtxoIdL1, InitializationTx | SettlementTx | RolloutTx | FinalizationTx | DeinitTx],

        /** Fallback txs, indexed by utxo id of [[FallbackTx.treasurySpent]]. By construction, there
          * may be exactly one fallback tx for any utxo id.
          */
        fallbackTxs: mutable.Map[UtxoIdL1, FallbackTx],

        /** We need to know whether finalization happened to be able to decide on the absent
          * treasury state.
          */
        finalizationTxSeq: Ref[IO, Option[FinalizationTxSeq]]
    )

    /** Finds tx that spends the [[utxoToSpend]] and all that follow it.
      * @param utxoToSpend
      * @return
      */
    def getSequence(
        utxoToSpend: UtxoIdL1
    ): Either[Errors.NoSpendingTxFound.type, List[Transaction]] = ???

    // ===================================
    // Main loop
    // ===================================

    /** TODO: this should be called periodically somehow, or an external timer should send us a
      * message via the inbox.
      */

    // ===================================
    // Actions
    // ===================================

    /** Finds the deinit tx which is always the last one and submits it.
      *
      * @param treasury
      */

    /** Finds the rollout sequence starting from a particular utxo and submits it.
      * @param rolloutUtxos
      *   rollout utxos found
      */

    // ===================================
    // Errors
    // ===================================

    enum Errors:
        /** The state already contains a fallback tx for a particular utxo. */
        case NonUniqueFallbackTx()

        /** There is a utxo for spending, but there is no tx that can spend it in the state. */
        case NoSpendingTxFound()

}
