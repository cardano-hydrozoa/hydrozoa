package hydrozoa.l2.ledger.state

import hydrozoa.OutputRefL2
import hydrozoa.infra.decodeBech32AddressL2
import scalus.builtin.ByteString
import scalus.prelude.Maybe.Nothing
import scalus.ledger.api.v1 as scalus

import scala.collection.mutable

opaque type OutputRefInt = scalus.TxOutRef
opaque type OutputInt = scalus.TxOut

def liftOutputRef(outputRefL2: OutputRefL2): OutputRefInt =
    val sTxId = scalus.TxId(ByteString.fromHex(outputRefL2.txId.hash))
    val sTxIx = BigInt(outputRefL2.outputIx.ix)
    scalus.TxOutRef(sTxId, sTxIx)

def unwrapTxIn(outputRef: OutputRefInt): scalus.TxOutRef = outputRef

def liftOutput(bech32: hydrozoa.AddressBechL2, coins: BigInt): OutputInt =
    val address = decodeBech32AddressL2(bech32)
    val value = scalus.Value.lovelace(coins)
    scalus.TxOut(address = address, value = value, datumHash = Nothing)

def unwrapTxOut(output: OutputInt): scalus.TxOut = output

type Utxos = mutable.Map[OutputRefInt, OutputInt]
type UtxosDiff = Set[(OutputRefInt, OutputInt)]
type UtxosDiffMutable = mutable.Set[(OutputRefInt, OutputInt)]

def checkSumInvariant(inputs: List[OutputInt], outputs: List[OutputInt]): Boolean =
    val before: scalus.Value = inputs.map(_.value).fold(scalus.Value.zero)(scalus.Value.plus)
    val after: scalus.Value = outputs.map(_.value).fold(scalus.Value.zero)(scalus.Value.plus)
    before == after
