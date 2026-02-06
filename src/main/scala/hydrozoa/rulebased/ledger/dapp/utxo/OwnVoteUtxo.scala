package hydrozoa.rulebased.ledger.dapp.utxo

import scalus.cardano.ledger.{AddrKeyHash, Utxo}

final case class OwnVoteUtxo(
    voter: AddrKeyHash,
    utxo: Utxo
)

object OwnVoteUtxo {}

final case class VoteUtxoCast(utxo: Utxo)
