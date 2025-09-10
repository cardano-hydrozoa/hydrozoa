package hydrozoa.multisig.ledger.l1.utxo

import hydrozoa.{L1, Utxo}
import hydrozoa.multisig.ledger.l1.token.Token.voteTokenName

final case class VoteUtxo(utxo: Utxo[L1])

object VoteUtxo {}
