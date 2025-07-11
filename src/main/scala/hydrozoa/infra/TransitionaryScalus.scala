// These types are temporary bridging between hydrozoa types and scalus types.
// Eventually, we want to move exclusively to the scalus types

package hydrozoa.infra.transitionary

import com.bloxbean.cardano.client.backend.api.BackendService
import com.bloxbean.cardano.client.plutus.spec.PlutusData
import com.bloxbean.cardano.client.util.HexUtil
import hydrozoa.infra.{Piper, toEither}
import hydrozoa.{AnyLevel, NativeScript, UtxoId, Network as HNetwork}
import scalus.bloxbean.Interop
import scalus.builtin.ByteString
import scalus.cardano.address.Network.{Mainnet, Testnet}
import scalus.cardano.address.{Address, Network}
import scalus.cardano.ledger.BloxbeanToLedgerTranslation.toLedgerValue
import scalus.cardano.ledger.Script.Native
import scalus.cardano.ledger.*

val emptyTxBody: TransactionBody = TransactionBody(
  inputs = Set.empty,
  outputs = IndexedSeq.empty,
  fee = Coin(0),
  ttl = None,
  certificates = Set.empty,
  withdrawals = None,
  auxiliaryDataHash = None,
  validityStartSlot = None,
  mint = None,
  scriptDataHash = None,
  collateralInputs = Set.empty,
  requiredSigners = Set.empty,
  networkId = None,
  collateralReturnOutput = None,
  totalCollateral = None,
  referenceInputs = Set.empty,
  votingProcedures = None,
  proposalProcedures = Set.empty,
  currentTreasuryValue = None,
  donation = None
)

extension [L <: AnyLevel](utxo: UtxoId[L]) {
    def toScalus: TransactionInput =
        TransactionInput(
          transactionId = Hash(ByteString.fromHex(utxo.txId.hash)),
          index = utxo.outputIx.ix
        )
}

extension [HF, P](hash: Hash[HF, P]) {
    def toIArray: IArray[Byte] =
        IArray.from(hash.bytes)
}

extension (native: Native) {
    def toHydrozoaNativeScript: NativeScript = {
        NativeScript(native.script.toCbor)
    }
}

extension (network: HNetwork) {
    def toScalus: Network = {
        if network.networkId == 1
        then Mainnet
        else Testnet
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

                println(utxo.getAddress)
                val outAddress = Address.fromBech32(utxo.getAddress)
                val outVal: Value = utxo.toValue.toLedgerValue
                val outDat = Option(utxo.getInlineDatum).map(hex =>
                    hex |> HexUtil.decodeHexString
                        |> PlutusData.deserialize
                        |> Interop.toScalusData
                        |> DatumOption.Inline.apply
                )

                TransactionOutput(
                  address = outAddress,
                  value = outVal,
                  datumOption = outDat
                )
            })
    }

}
