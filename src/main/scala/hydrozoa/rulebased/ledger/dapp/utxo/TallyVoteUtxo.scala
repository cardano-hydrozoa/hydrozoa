package hydrozoa.rulebased.ledger.dapp.utxo

import hydrozoa.{L1, Utxo}

/** This represents a vote utxo in tallying.
  */
final case class TallyVoteUtxo(
    utxo: Utxo[L1]
)

object TallyVoteUtxo {}
