package hydrozoa.l2.ledger.state

import hydrozoa.{L2, Output, UtxoIdL2, TxId, TxIx}
import hydrozoa.infra.{decodeBech32AddressL2, extractAddress}
import scalus.builtin.ByteString
import scalus.prelude.Maybe.Nothing

import scala.collection.mutable
import scalus.prelude.AssocMap
import scalus.prelude.Maybe.Just
import scalus.prelude.Prelude.given_Eq_ByteString
import scalus.ledger.api.v1 as scalus

opaque type OutputRefInt = scalus.TxOutRef
opaque type OutputInt = scalus.TxOut

def liftOutputRef(UtxoIdL2: UtxoIdL2): OutputRefInt =
    val sTxId = scalus.TxId(ByteString.fromHex(UtxoIdL2.txId.hash))
    val sTxIx = BigInt(UtxoIdL2.outputIx.ix)
    scalus.TxOutRef(sTxId, sTxIx)

def unliftOutputRef(outputRef: OutputRefInt): UtxoIdL2 =
    UtxoIdL2(TxId(outputRef.id.toString), TxIx(outputRef.idx.longValue))

def unwrapTxIn(outputRef: OutputRefInt): scalus.TxOutRef = outputRef

def liftOutput(bech32: hydrozoa.AddressBechL2, coins: BigInt): OutputInt =
    val address = decodeBech32AddressL2(bech32)
    val value = scalus.Value.lovelace(coins)
    scalus.TxOut(address = address, value = value, datumHash = Nothing)

def unliftOutput(output: OutputInt): Output[L2] =
    val Just(e) = AssocMap.lookup(output.value)(ByteString.empty)
    val Just(coins) = AssocMap.lookup(e)(ByteString.empty)
    Output[L2](extractAddress(output.address).asL1, coins)

def unwrapTxOut(output: OutputInt): scalus.TxOut = output

type UtxosSetOpaque = Map[OutputRefInt, OutputInt]
type UtxosSetOpaqueMutable = mutable.Map[OutputRefInt, OutputInt]

def checkSumInvariant(inputs: List[OutputInt], outputs: List[OutputInt]): Boolean =
    val before: scalus.Value = inputs.map(_.value).fold(scalus.Value.zero)(scalus.Value.plus)
    val after: scalus.Value = outputs.map(_.value).fold(scalus.Value.zero)(scalus.Value.plus)
    before == after
