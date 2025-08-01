package hydrozoa.l1.multisig.tx.initialization

import com.bloxbean.cardano.client.backend.api.BackendService
import hydrozoa.infra.transitionary.{bloxToScalusUtxoQuery, emptyTxBody, toScalus}
import hydrozoa.l1.multisig.onchain.{mkBeaconTokenName, mkHeadNativeScript}
import hydrozoa.l1.multisig.state.mkInitMultisigTreasuryDatum
import hydrozoa.l1.multisig.tx.InitTx
import hydrozoa.{AddressBech, AddressBechL1, Tx}
import io.bullet.borer.Cbor
import scalus.builtin.ByteString
import scalus.builtin.Data.toData
import scalus.cardano.address.ShelleyDelegationPart.Null
import scalus.cardano.address.{Address, ShelleyAddress, ShelleyPaymentPart}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.DatumOption.Inline

import scala.collection.immutable.SortedMap

class ScalusInitializationTxBuilder(backendService: BackendService) extends InitTxBuilder {

    override def mkInitializationTxDraft(
        recipe: InitTxRecipe
    ): Either[String, (InitTx, AddressBechL1)] =
        bloxToScalusUtxoQuery(backendService, recipe.seedUtxo.toScalus) match {
            case Left(err) => Left("Scalus InititializationTxBuilder failed: " ++ err)
            case Right(seedOutput) =>
                Right({
                    // TODO: we set the fee to 1 ada, but this doesn't need to be
                    val feeCoin = Coin(1_000_000)

                    // Construct head native script directly from the list of peers
                    val headNativeScript = mkHeadNativeScript(recipe.peers)

                    // Put the head address of the native script
                    val headAddress: Address = (
                      ShelleyAddress(
                        network = recipe.network.toScalus,
                        payment = ShelleyPaymentPart.Script(headNativeScript.scriptHash),
                        delegation = Null
                      )
                    )

                    // singleton beacon token minted by the native script with the TN being the hash of the
                    // seed utxo
                    // TODO: factor out "mkSingleToken"
                    val beaconToken: MultiAsset = MultiAsset(
                      SortedMap.from(
                        Seq(
                          (
                            headNativeScript.scriptHash,
                            SortedMap.from(
                              Seq(
                                (
                                  AssetName(
                                    ByteString.fromHex(
                                      mkBeaconTokenName(recipe.seedUtxo).tokenNameHex.drop(2)
                                    )
                                  ),
                                  1L
                                )
                              )
                            )
                          )
                        )
                      )
                    )

                    // Head output (L1) sits at the head address with the initial deposit from the seed utxo
                    // and beacon, as well as the initial datum.
                    val headValue: Value =
                        Value(coin = Coin(recipe.coins.toLong), multiAsset = beaconToken)
                    val headOutput: TransactionOutput =
                        TransactionOutput(
                          address = headAddress,
                          value = headValue,
                          datumOption = Some(Inline(mkInitMultisigTreasuryDatum.toData))
                        )

                    val changeOutput: TransactionOutput = TransactionOutput(
                      address = seedOutput.address,
                      // Change is calculated manually here as the seed output's value, minus the
                      // ada put into the head, minus the fee.
                      value = seedOutput.value -
                          Value(coin = Coin(recipe.coins.toLong)) -
                          Value(
                            coin = feeCoin,
                          ),
                      datumOption = None
                    )

                    val ourBody =
                        emptyTxBody.copy(
                          inputs = Set(recipe.seedUtxo.toScalus),
                          outputs = IndexedSeq(headOutput, changeOutput).map(Sized(_)),
                          // TODO: we set the fee to 1 ada, but this doesn't need to be
                          fee = feeCoin,
                          mint = Some(Mint(beaconToken))
                        )

                    val scalusTransaction: Transaction = Transaction(
                      body = KeepRaw(ourBody),
                      witnessSet = TransactionWitnessSet(nativeScripts = Set(headNativeScript)),
                      isValid = true,
                      auxiliaryData = None
                    )

                    (
                      Tx(Cbor.encode(scalusTransaction).toByteArray),
                      headAddress match {
                          case sa : ShelleyAddress => AddressBech(sa.toBech32.get)
                          // NOTE (Peter, 2025-08-07) I miss monads, how do I do those in scala?
                          case _ => return Left("Hydra Head is not at a Shelly address")
                      }
                    )

                })

        }
}
