package hydrozoa.multisig.ledger.dapp.utxo

import hydrozoa.{L1, Utxo}
import hydrozoa.multisig.ledger.dapp.token.Token.voteTokenName

final case class VoteUtxo(utxo: Utxo[L1])

object VoteUtxo {}
