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
import hydrozoa.node.state.HeadStateReader
import io.bullet.borer.Cbor
import scalus.cardano.ledger.Script.Native
import scalus.cardano.ledger.*
import hydrozoa.node.state.multisigRegime
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.prelude.log

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
        val beaconTokenBurn = Map((beaconTokenPolicyId, Map((beaconTokenName, -1.toLong))))

        val treasuryUtxoId = reader.multisigRegime(_.treasuryUtxoId)
        val treasuryUtxo: TransactionOutput =
            bloxToScalusUtxoQuery(backendService, treasuryUtxoId) match {
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
                        b.copy(value = b.value.copy(assets = Map.empty))
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
        val changeValue = (treasuryUtxo.value
            - valueWithdrawn
            - Value(Coin(0.toLong))
            - Value(feeCoin))
            .copy(assets = Map.empty)

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

/*

[
    {   // Inputs (treasury)
        0: 258_1([
            [
                h'11668e318ef64672ff8854c0c31b8b8ffcc4e8735aee0dc69d08386dde777542',
                0,
            ],
        ]),
        // outputs (to who?)
        1: [
            [
                h'00c8c47610a36034aac6fc58848bdae5c278d994ff502c05455e3b3ee8f8ed3a0eea0ef835ffa7bbfcde55f7fe9d2cc5d55ea62cecb42bab3c',
                99429489_2,
            ],
            [
                h'00fc576df3a279885a7a4d0fc500372daa1d96f26c6763542ecd2ad8551753024adea37c134edebb68dc0cfaed5a7009e8305fe1fed8d0ccd1',
                100000000_2,
            ],
        ],
        // fee
        2: 190449_2,
        // Mint: burn beacon token
        9: {
            h'874bfae36ec944b5444f02055df3c7719be9459679986ef8e4b87b41': {
                h'01349900f3dd3a33673fbe745de717783a685793fcd81c6dd29fe7ab8c9c53cb': -1,
            },
        },
    },
    {   // native script witness
        1: 258_1([
            [
                1,
                [
                    [
                        0,
                        h'07d781fe8e33883e371f9550c2f1087321fc32e06e80b65e349ccb02',
                    ],
                    [
                        0,
                        h'4048ff89ca4f88e66598e620aa0c7128c2145d9a181ae9a4a81ca8e3',
                    ],
                    [
                        0,
                        h'c8c47610a36034aac6fc58848bdae5c278d994ff502c05455e3b3ee8',
                    ],
                    [
                        0,
                        h'ca6e1b1f320d543a24adeabc0aa4627635c7349b639f86f74bdfdd78',
                    ],
                ],
            ],
        ]),
    },
    true,
    null,
]

 */
