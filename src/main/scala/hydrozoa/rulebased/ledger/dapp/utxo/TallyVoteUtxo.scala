package hydrozoa.rulebased.ledger.dapp.utxo

import hydrozoa.{L1, Utxo}
import scalus.cardano.ledger.AddrKeyHash

/** This represents a vote utxo in tallying.
  */
final case class TallyVoteUtxo(
    voter: AddrKeyHash,
    utxo: Utxo[L1]
)

object TallyVoteUtxo {}
