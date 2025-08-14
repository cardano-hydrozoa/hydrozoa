package hydrozoa.l1.multisig.tx.initialization

import com.bloxbean.cardano.client.backend.api.BackendService
import hydrozoa.infra.transitionary.{bloxToScalusUtxoQuery, emptyTxBody}
import hydrozoa.l1.multisig.onchain.{mkBeaconTokenName, mkHeadNativeScript}
import hydrozoa.l1.multisig.state.mkInitMultisigTreasuryDatum
import hydrozoa.l1.multisig.tx.InitTx
import hydrozoa.{Address, AddressL1, L1, Tx}
import scalus.builtin.Data.toData
import scalus.cardano.address.ShelleyDelegationPart.Null
import scalus.cardano.address.{ShelleyAddress, ShelleyPaymentPart}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.rules.UtxoEnv
import scalus.cardano.ledger.txbuilder.OnSurplus
import scalus.cardano.ledger.utils.TxBalance

import scala.collection.immutable.SortedMap

class ScalusInitializationTxBuilder(backendService: BackendService) extends InitTxBuilder {

    override def mkInitializationTxDraft(
        recipe: InitTxRecipe
    ): Either[String, (InitTx, AddressL1)] =
        for
            seedOutput <- bloxToScalusUtxoQuery(backendService, recipe.seedUtxo)
            // Construct head native script directly from the list of peers
            headNativeScript = mkHeadNativeScript(recipe.peers)

            // Put the head address of the native script
            headAddress =
                ShelleyAddress(
                  network = recipe.network,
                  payment = ShelleyPaymentPart.Script(headNativeScript.scriptHash),
                  delegation = Null
                )

            // singleton beacon token minted by the native script with the TN being the hash of the
            // seed utxo
            // TODO: factor out "mkSingleToken"
            beaconToken: MultiAsset = MultiAsset(
              SortedMap(
                headNativeScript.scriptHash -> SortedMap(
                  mkBeaconTokenName(recipe.seedUtxo) -> 1L
                )
              )
            )

            // Head output (L1) sits at the head address with the initial deposit from the seed utxo
            // and beacon, as well as the initial datum.
            headValue: Value =
                Value(coin = recipe.coins, multiAsset = beaconToken)
            headOutput: TransactionOutput =
                TransactionOutput(
                  address = headAddress,
                  value = headValue,
                  datumOption = Some(Inline(mkInitMultisigTreasuryDatum.toData))
                )

            // Construct and balance

            unbalancedBody =
                emptyTxBody.copy(
                  inputs = Set(recipe.seedUtxo),
                  outputs = IndexedSeq(headOutput).map(Sized(_)),
                  // TODO: we set the fee to 1 ada, but this doesn't need to be
                  fee = Coin(0L),
                  mint = Some(Mint(beaconToken))
                )

            unbalancedTransaction: Transaction = Transaction(
              body = KeepRaw(unbalancedBody),
              witnessSet = TransactionWitnessSet(nativeScripts = Set(headNativeScript)),
              isValid = true,
              auxiliaryData = None
            )

            balancedTransaction = TxBalance.doBalance(unbalancedTransaction)(
              (Map(recipe.seedUtxo.untagged -> seedOutput)),
              UtxoEnv.default.params,
              OnSurplus.toAddress(seedOutput.address)
            )
        yield (
          Tx(balancedTransaction),
          Address[L1](headAddress)
        )

}
