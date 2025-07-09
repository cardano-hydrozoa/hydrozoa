// These types are temporary bridging between hydrozoa types and scalus types.
// Eventually, we want to move exclusively to the scalus types

package hydrozoa.infra.transitionary

import co.nstant.in.cbor.CborDecoder
import com.bloxbean.cardano.client.backend.api.BackendService
import hydrozoa.infra.toEither
import hydrozoa.{AddressBechL1, NativeScript, AnyLevel, UtxoId, UtxoIdL1, Network as HNetwork}
import scalus.builtin.ByteString
import scalus.builtin.Data.toData
import scalus.cardano.ledger.{DatumOption, Hash, TransactionInput, TransactionOutput, Value}
import scalus.cardano.address.{Address, Network, ShelleyAddress}
import scalus.cardano.ledger.BloxbeanToLedgerTranslation.toLedgerValue
import scalus.cardano.ledger.Script.Native
import scalus.cardano.ledger.TransactionOutput.Babbage
import co.nstant.in.cbor.model.Array

extension [L <: AnyLevel] (utxo: UtxoId[L]) {
    def toScalus: TransactionInput =
        TransactionInput(
          transactionId = Hash(ByteString.fromHex(utxo.txId.hash)),
          index = utxo.outputIx.ix
        )
}

extension [HF, P] (hash : Hash[HF, P]){
    def toIArray : IArray[Byte] =
        IArray.from(hash.bytes)
}

extension (native : Native) {
    def toHydrozoaNativeScript: NativeScript = {
        NativeScript(native.script.toCbor)
    }
}

// Uses the bloxbean backend to query a utxo into a scalus TransactionOutput
def bloxToScalusUtxoQuery[L <: AnyLevel](
    backendService: BackendService,
    utxoId: UtxoId[L]
): Either[String, TransactionOutput] = {
    backendService.getUtxoService
        .getTxOutput(utxoId.txId.hash, utxoId.outputIx.ix.intValue)
        .toEither match {
        case Left(err) => Left("[bloxToScalusUtxoQuery]: Querying failed: " ++ err)
        case Right(utxo) =>
            Right({

                // FIXME: No idea if this is correct
                val outAddress = Address.fromBech32(utxo.getAddress)
                val outVal: Value = utxo.toValue.toLedgerValue
                val outDat = utxo.getInlineDatum

                TransactionOutput(
                  address = outAddress,
                  value = outVal,
                  datumOption = Some(DatumOption.Inline(toData(ByteString.fromHex(outDat))))
                )
            })
    }

}
