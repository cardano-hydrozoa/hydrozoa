package hydrozoa.rulebased.ledger.l1.utxo

import scalus.cardano.ledger.{AddrKeyHash, Utxo}

final case class OwnVoteUtxo(
    voter: AddrKeyHash,
    utxo: Utxo
)

object OwnVoteUtxo {}

final case class VoteUtxoCast(utxo: Utxo)
