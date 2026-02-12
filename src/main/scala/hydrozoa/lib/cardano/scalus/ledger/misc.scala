package hydrozoa.lib.cardano.scalus.ledger

import scalus.cardano.ledger.{Utxo, Utxos}

extension (self: List[Utxo]) def asUtxos: Utxos = self.map(u => u.input -> u.output).toMap

extension (self: Utxos) def asUtxoList: List[Utxo] = self.toList.map(Utxo.apply)
