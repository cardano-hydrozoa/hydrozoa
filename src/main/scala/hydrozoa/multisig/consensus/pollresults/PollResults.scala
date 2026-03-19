package hydrozoa.multisig.consensus.pollresults

import scalus.cardano.ledger.TransactionInput

/** So-called "poll results" from the Cardano Liaison, i.e., a set of all utxos ids found at the
  * multisig head address.
  *
  * @param utxos
  *   all utxos found
  */
final case class PollResults(utxos: Set[TransactionInput])

object PollResults:
    val empty: PollResults = PollResults(Set.empty)
