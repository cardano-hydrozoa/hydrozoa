package hydrozoa.l1.multisig.tx.finalization

import com.bloxbean.cardano.client.backend.api.BackendService
import com.typesafe.scalalogging.Logger
import hydrozoa.Tx
import hydrozoa.infra.transitionary.{
    bloxToScalusUtxoQuery,
    emptyTxBody,
    toScalus,
    toScalusNativeScript
}
import hydrozoa.l1.multisig.tx.FinalizationTx
import hydrozoa.node.state.{HeadStateReader, multisigRegime}
import io.bullet.borer.Cbor
import scalus.cardano.ledger.*
import scalus.cardano.ledger.Script.Native
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.prelude.log

import scala.collection.immutable.SortedMap

class ScalusFinalizationTxBuilder(
    backendService: BackendService,
    reader: HeadStateReader
) extends FinalizationTxBuilder {

    override def buildFinalizationTxDraft(
        r: FinalizationRecipe
    ): Either[String, FinalizationTx] = {
        val feeCoin: Coin = Coin(1000000)

        val headNativeScript: Native =
            reader.multisigRegime(_.headNativeScript).toScalusNativeScript

        val beaconTokenPolicyId: PolicyId = headNativeScript.scriptHash
        val beaconTokenName: AssetName =
            reader.multisigRegime(_.beaconTokenName).toScalus
        val beaconTokenBurn = Mint(
          MultiAsset(
            SortedMap.from(
              Seq((beaconTokenPolicyId, SortedMap.from(Seq((beaconTokenName, -1.toLong)))))
            )
          )
        )

        val treasuryUtxoId = reader.multisigRegimeReader(_.treasuryUtxoId)

        val treasuryUtxo: TransactionOutput =
            bloxToScalusUtxoQuery(backendService, treasuryUtxoId.toScalus) match {
                case Left(err) => return Left("treasury utxo not found")
                case Right(to) => to
            }

        // The L2 utxos will have beacon tokens in them. We want to only get the ada from them
        val outputsToWithdraw =
            r.utxosWithdrawn.utxoMap
                .map(w => w._2.toScalus)
                .map({
                    // Take only the added for the outputs
                    case b: Babbage =>
                        b.copy(value = b.value.copy(assets = MultiAsset.empty))
                            .asInstanceOf[TransactionOutput]
                    case _ => return Left("L2 utxo not a babbage UTxO")
                })
        val valueWithdrawn: Value =
            outputsToWithdraw.foldLeft(Value.zero)((s, w) => s + w.value)

        val changeAddress = reader.multisigRegime(_.seedAddress).toScalus

        // Change output value is:
        // The treasury value,
        //    minus the ADA value of all withdrawn UTxOs,
        //    minus the beacon token from the treasury (which is burned),
        //    minus the fee)
        // Note that we have to explicitly set the "assets" field to empty, because
        // we will get a runtime exception if any of the multiassets are 0.
        // We can't just subtract the beacon token
        val changeValue =
            try {
                (treasuryUtxo.value
                    - valueWithdrawn
                    - Value(feeCoin))
                    .copy(assets = MultiAsset.empty)
            } catch {
                case _: IllegalArgumentException => return Left("Illegal change value found")
            }

        val changeOutput: TransactionOutput = TransactionOutput(
          address = changeAddress,
          value = changeValue,
          datumOption = None
        )

        val txBody =
            emptyTxBody.copy(
              inputs = Set(treasuryUtxoId.toScalus),
              outputs = outputsToWithdraw.toIndexedSeq.map(Sized(_)).appended(Sized(changeOutput)),
              // TODO: we set the fee to 1 ada, but this doesn't need to be
              fee = feeCoin,
              mint = Some(beaconTokenBurn)
            )

        val scalusTransaction: Transaction = Transaction(
          body = KeepRaw(txBody),
          witnessSet = TransactionWitnessSet(nativeScripts = Set(headNativeScript)),
          isValid = true,
          auxiliaryData = None
        )

        Right(Tx(Cbor.encode(scalusTransaction).toByteArray))
    }

}
