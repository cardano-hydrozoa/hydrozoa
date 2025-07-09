package hydrozoa.l1.multisig.tx.initialization

import com.bloxbean.cardano.client.backend.api.BackendService
import hydrozoa.infra.{mkBuilder, numberOfSignatories, toEither}
import hydrozoa.l1.multisig.onchain.{mkBeaconTokenName, mkHeadNativeScriptScalus}
import hydrozoa.l1.multisig.state.mkInitMultisigTreasuryDatum
import hydrozoa.l1.multisig.tx.{InitTx, MultisigTx}
import hydrozoa.{AddressBech, AddressBechL1, L1, Tx, TxL1, UtxoId, UtxoIdL1}
import scalus.builtin.Data.toData
import scalus.|>

import java.math.BigInteger
import scala.jdk.CollectionConverters.*
import scalus.cardano.ledger.*
import scalus.cardano.address.{Address, ShelleyAddress, ShelleyPaymentPart}
import hydrozoa.infra.transitionary.{bloxToScalusUtxoQuery, toScalus}
import io.bullet.borer.Cbor
import scalus.builtin.ByteString
import scalus.cardano.address.Address.Shelley
import scalus.cardano.address.Network.Mainnet
import scalus.cardano.address.ShelleyDelegationPart.Null
import scalus.cardano.ledger.DatumOption.Inline

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
class ScalusInitializationTxBuilder(backendService: BackendService) extends InitTxBuilder {

    override def mkInitializationTxDraft(
        recipe: InitTxRecipe
    ): Either[String, (InitTx, AddressBechL1)] =
        bloxToScalusUtxoQuery(backendService, recipe.seedUtxo) match {
            case Left(err) => Left("Scalus InititializationTxBuilder failed: " ++ err)
            case Right(seedOutput) =>
                Right({
                    // TODO: we set the fee to 1 ada, but this doesn't need to be
                    val feeCoin = Coin(1_000_000)

                    // Construct head native script directly from the list of peers
                    val headNativeScript = mkHeadNativeScriptScalus(recipe.peers)

                    // Put the head address of the native script
                    val headAddress: Address = Shelley(
                      ShelleyAddress(
                        // QUESTION (Peter, 2025-07-09): Is this correct? I believe someone once mentioned we
                        // wanted the mainnet network for l2
                        network = Mainnet,
                        payment = ShelleyPaymentPart.Script(headNativeScript.scriptHash),
                        delegation = Null
                      )
                    )

                    // singleton beacon token minted by the native script with the TN being the hash of the
                    // seed utxo
                    val beaconToken: MultiAsset = Map(
                      (
                        headNativeScript.scriptHash,
                        Map(
                          (
                            AssetName(
                              ByteString.fromHex(mkBeaconTokenName(recipe.seedUtxo).tokenNameHex)
                            ),
                            1
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
                      value = seedOutput.value - headValue - Value(
                        coin = feeCoin,
                        multiAsset = Map.empty
                      ),
                      datumOption = None
                    )

                    val ourBody =
                        emptyTxBody.copy(
                          inputs = Set(recipe.seedUtxo.toScalus),
                          outputs = IndexedSeq(headOutput, changeOutput).map(Sized(_)),
                          // TODO: we set the fee to 1 ada, but this doesn't need to be
                          fee = feeCoin,
                          mint = Some(beaconToken)
                        )

                    val scalusTransaction : Transaction =         Transaction(
                        body = KeepRaw(ourBody),
                        witnessSet = TransactionWitnessSet(nativeScripts = Set(headNativeScript)),
                        isValid = true,
                        auxiliaryData = None
                    )

                    (Tx(Cbor.encode(scalusTransaction).toByteArray),

                      headAddress match {
                          case Shelley(sa) => AddressBech(sa.toBech32.get)
                          // NOTE (Peter, 2025-08-07) I miss monads, how do I do those in scala?
                          case _ => throw new Exception ("Hydra Head is not at a Shelly address")
                      }
                    )

                })

        }
}
