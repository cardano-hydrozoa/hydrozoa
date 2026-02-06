package hydrozoa.rulebased.ledger.dapp.utxo

import scalus.cardano.ledger.Utxo

/** This represents a vote utxo in tallying.
  */
final case class TallyVoteUtxo(
    utxo: Utxo
)

object TallyVoteUtxo {}
