package hydrozoa.l2.ledger.state

import cats.implicits.toBifunctorOps
import hydrozoa.{L2, Output, OutputL2, TxId, TxIx, UtxoIdL2}
import hydrozoa.infra.{decodeBech32AddressL1, decodeBech32AddressL2, extractAddress}
import scala.collection.mutable

import scalus.builtin.ByteString
import scalus.prelude.Maybe.Nothing
import scalus.prelude.AssocMap
import scalus.prelude.Maybe.Just
import scalus.prelude.Prelude.given_Eq_ByteString
import scalus.ledger.api.v1 as scalus

opaque type OutputRefInt = scalus.TxOutRef
opaque type OutputInt = scalus.TxOut

def liftUtxoSet(utxoSet: Map[UtxoIdL2, OutputL2]): UtxosSetOpaque =
    utxoSet.map(_.bimap(liftOutputRef, liftOutput_))

def liftOutputRef(UtxoIdL2: UtxoIdL2): OutputRefInt =
    val sTxId = scalus.TxId(ByteString.fromHex(UtxoIdL2.txId.hash))
    val sTxIx = BigInt(UtxoIdL2.outputIx.ix)
    scalus.TxOutRef(sTxId, sTxIx)

def unliftOutputRef(outputRef: OutputRefInt): UtxoIdL2 =
    UtxoIdL2(TxId(outputRef.id.hash.toHex), TxIx(outputRef.idx.longValue))

def unwrapTxIn(outputRef: OutputRefInt): scalus.TxOutRef = outputRef

def liftOutput_(output: OutputL2): OutputInt =
    val address = decodeBech32AddressL1(output.address)
    val value = scalus.Value.lovelace(output.coins)
    scalus.TxOut(address = address, value = value, datumHash = Nothing)

// FIXME: remove, use liftOutput_
def liftOutput(bech32: hydrozoa.AddressBechL2, coins: BigInt): OutputInt =
    liftOutput_(Output(bech32.asL1, coins))

def unliftOutput(output: OutputInt): Output[L2] =
    val Just(e) = AssocMap.lookup(output.value)(ByteString.empty)
    val Just(coins) = AssocMap.lookup(e)(ByteString.empty)
    Output[L2](extractAddress(output.address).asL1, coins)

def unliftUtxoSet(utxosSetOpaque: UtxosSetOpaque): Map[UtxoIdL2, OutputL2] =
    utxosSetOpaque.map(_.bimap(unliftOutputRef, unliftOutput))

def unwrapTxOut(output: OutputInt): scalus.TxOut = output

type UtxosSetOpaque = Map[OutputRefInt, OutputInt]
type UtxosSetOpaqueMutable = mutable.Map[OutputRefInt, OutputInt]

def checkSumInvariant(inputs: List[OutputInt], outputs: List[OutputInt]): Boolean =
    val before: scalus.Value = inputs.map(_.value).fold(scalus.Value.zero)(scalus.Value.plus)
    val after: scalus.Value = outputs.map(_.value).fold(scalus.Value.zero)(scalus.Value.plus)
    before == after
