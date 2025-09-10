package hydrozoa.multisig.ledger.l1.real.utxo

import hydrozoa.{L1, Utxo}
import hydrozoa.multisig.ledger.l1.real.token.Token.rolloutTokenName

final case class RolloutUtxo(utxo: Utxo[L1])

object RolloutUtxo {}
