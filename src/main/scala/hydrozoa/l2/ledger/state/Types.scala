package hydrozoa.l2.ledger.state

import hydrozoa.infra.{decodeBech32AddressL2}
import hydrozoa.l2.ledger.SimpleOutput
import scalus.builtin.ByteString
import scalus.prelude.Maybe.Nothing
import scalus.ledger.api.v1 as scalus

import scala.collection.mutable

opaque type TxIn = scalus.TxOutRef
opaque type TxOut = scalus.TxOut

type Utxos = mutable.Map[TxIn, TxOut]

type UtxosDiff = Set[(TxIn, TxOut)]
type MutableUtxosDiff = mutable.Set[(TxIn, TxOut)]
type MutableOrderedUtxosDiff = mutable.Buffer[(TxIn, TxOut)]
type OrderedUtxosDiff = List[(TxIn, TxOut)]

// FIXME: move to another package
def mkTxIn(txId: hydrozoa.TxId, txIx: hydrozoa.TxIx): TxIn =
    val sTxId = scalus.TxId(ByteString.fromHex(txId.hash))
    val sTxIx = BigInt.apply(txIx.ix.intValue)
    scalus.TxOutRef(sTxId, sTxIx)

def unwrapTxIn(txIn: TxIn): scalus.TxOutRef = txIn

def mkTxOut(bech32: hydrozoa.AddressBechL2, coins: BigInt): TxOut =
    val address = decodeBech32AddressL2(bech32)
    val value = scalus.Value.lovelace(coins)
    scalus.TxOut(address = address, value = value, datumHash = Nothing)

def unwrapTxOut(txOut: TxOut): scalus.TxOut = txOut

def checkSumInvariant(inputs: List[TxOut], outputs: List[TxOut]): Boolean =
    val before: scalus.Value = inputs.map(_.value).fold(scalus.Value.zero)(scalus.Value.plus)
    val after: scalus.Value = outputs.map(_.value).fold(scalus.Value.zero)(scalus.Value.plus)
    before == after
