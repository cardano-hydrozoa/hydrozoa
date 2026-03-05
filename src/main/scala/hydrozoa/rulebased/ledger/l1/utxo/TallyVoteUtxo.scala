package hydrozoa.rulebased.ledger.l1.utxo

import scalus.cardano.ledger.{AddrKeyHash, Utxo}

/** This represents a vote utxo in tallying.
  */
final case class TallyVoteUtxo(
    voter: AddrKeyHash,
    utxo: Utxo
)

object TallyVoteUtxo {}
