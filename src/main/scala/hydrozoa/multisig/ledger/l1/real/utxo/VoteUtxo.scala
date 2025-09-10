package hydrozoa.multisig.ledger.l1.real.utxo

import hydrozoa.{L1, Utxo}
import hydrozoa.multisig.ledger.l1.real.token.Token.voteTokenName

final case class VoteUtxo(utxo: Utxo[L1])

object VoteUtxo {}
