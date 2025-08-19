package hydrozoa.l1.multisig.tx.finalization

import com.bloxbean.cardano.client.backend.api.BackendService
import hydrozoa.infra.transitionary.{bloxToScalusUtxoQuery, emptyTxBody}
import hydrozoa.l1.multisig.tx.{FinalizationTx, MultisigTx}
import hydrozoa.node.state.{HeadStateReader, multisigRegime}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.Script.Native
import scalus.cardano.ledger.TransactionOutput.Babbage

import scala.collection.immutable.SortedMap
import scala.language.implicitConversions

class ScalusFinalizationTxBuilder(
    backendService: BackendService,
    reader: HeadStateReader
) extends FinalizationTxBuilder {

    override def buildFinalizationTxDraft(
        r: FinalizationRecipe
    ): Either[String, FinalizationTx] = {
        val feeCoin: Coin = Coin(1000000)

        val headNativeScript: Native =
            reader.multisigRegime(_.headNativeScript)

        val beaconTokenPolicyId: PolicyId = headNativeScript.scriptHash
        val beaconTokenName: AssetName =
            reader.multisigRegime(_.beaconTokenName)
        val beaconTokenBurn = Mint(
          MultiAsset(
            SortedMap.from(
              Seq((beaconTokenPolicyId, SortedMap.from(Seq((beaconTokenName, -1.toLong)))))
            )
          )
        )

        val treasuryUtxoId = reader.multisigRegimeReader(_.treasuryUtxoId)

        val treasuryUtxo: TransactionOutput =
            bloxToScalusUtxoQuery(backendService, treasuryUtxoId) match {
                case Left(err) => return Left("treasury utxo not found")
                case Right(to) => to
            }

        // The L2 utxos will have beacon tokens in them. We want to only get the ada from them
        val outputsToWithdraw =
            r.utxosWithdrawn.values
                .map(b => b.copy(value = b.value.copy(assets = MultiAsset.empty)))
        val valueWithdrawn: Value =
            outputsToWithdraw.foldLeft(Value.zero)((s, w) => s + w.value)

        val changeAddress = reader.multisigRegime(_.seedAddress)

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
              inputs = Set(treasuryUtxoId),
              outputs = outputsToWithdraw.toIndexedSeq
                  .map((b: Babbage) => Sized(b.asInstanceOf[TransactionOutput]))
                  .appended(Sized(changeOutput)),
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

        Right(MultisigTx(scalusTransaction))
    }

}
