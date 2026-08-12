package hydrozoa.multisig.consensus.pollresults

import com.suprnation.actor.DeadLetterSuppression
import scalus.cardano.ledger.TransactionInput

/** So-called "poll results" from the Cardano Liaison, i.e., a set of all utxos ids found at the
  * multisig head address.
  *
  * Fire-and-forget: the liaison keeps polling and fanning results out on a timer, so once a
  * recipient (e.g. a regime manager) has stopped, in-flight `PollResults` land as dead letters.
  * That is expected, not a fault, so it extends [[DeadLetterSuppression]] to keep the actor system
  * from logging every such delivery.
  *
  * @param utxos
  *   all utxos found
  */
final case class PollResults(utxos: Set[TransactionInput]) extends DeadLetterSuppression

object PollResults:
    val empty: PollResults = PollResults(Set.empty)
