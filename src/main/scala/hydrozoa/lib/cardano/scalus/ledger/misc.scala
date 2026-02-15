package hydrozoa.lib.cardano.scalus.ledger

import scalus.cardano.ledger.{ProtocolParams, Utxo, Utxos}

extension (self: List[Utxo]) def asUtxos: Utxos = self.map(u => u.input -> u.output).toMap

extension (self: Utxos) def asUtxoList: List[Utxo] = self.toList.map(Utxo.apply)

extension (self: ProtocolParams)
    def withZeroFees: ProtocolParams =
        self.copy(txFeeFixed = 0, txFeePerByte = 0)
