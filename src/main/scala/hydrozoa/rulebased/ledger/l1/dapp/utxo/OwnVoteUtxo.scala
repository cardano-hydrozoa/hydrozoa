package hydrozoa.rulebased.ledger.l1.dapp.utxo

import hydrozoa.L1
import hydrozoa.Utxo
import scalus.cardano.ledger.AddrKeyHash

final case class OwnVoteUtxo(
    voter: AddrKeyHash,
    utxo: Utxo[L1]
)

object OwnVoteUtxo {}

final case class VoteUtxoCast(utxo: Utxo[L1])
