package hydrozoa.l2.ledger.state

import hydrozoa.infra.decodeBech32AddressL2
import scalus.builtin.ByteString
import scalus.prelude.Maybe.Nothing
import scalus.ledger.api.v1 as scalus

import scala.collection.mutable

opaque type TxIn = scalus.TxOutRef
opaque type TxOut = scalus.TxOut

type UTxOs = mutable.Map[TxIn, TxOut]

def mkTxIn(txId: hydrozoa.TxId, txIx: hydrozoa.TxIx): TxIn =
    val sTxId = scalus.TxId(ByteString.fromHex(txId.hash))
    val sTxIx = BigInt.apply(txIx.ix.intValue)
    scalus.TxOutRef(sTxId, sTxIx)

def mkTxOut(bech32: hydrozoa.AddressBechL2, ada: Int): TxOut =
    val address = decodeBech32AddressL2(bech32)
    val value = scalus.Value.lovelace(ada * 1_000_000)
    scalus.TxOut(address = address, value = value, datumHash = Nothing)
