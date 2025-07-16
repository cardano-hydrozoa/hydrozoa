package hydrozoa.l1.multisig.tx.initialization

import com.bloxbean.cardano.client.backend.api.BackendService
import hydrozoa.*
import hydrozoa.infra.TxBuilder
import hydrozoa.infra.transitionary.{bloxToScalusUtxoQuery, toScalus}
import hydrozoa.l1.multisig.onchain.{mkBeaconTokenName, mkHeadNativeScript}
import hydrozoa.l1.multisig.state.mkInitMultisigTreasuryDatum
import hydrozoa.l1.multisig.tx.InitTx
import scalus.builtin.ByteString
import scalus.cardano.address.Address.Shelley
import scalus.cardano.address.ShelleyDelegationPart.Null
import scalus.cardano.address.{Address, ShelleyAddress, ShelleyPaymentPart}
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.Script.Native
import scalus.cardano.ledger.{
    AssetName,
    Coin,
    KeepRaw,
    MultiAsset,
    Sized,
    Transaction,
    TransactionInput,
    TransactionOutput,
    TransactionWitnessSet,
    Value
}
import scalus.builtin.Data.toData

import scala.util.control.TailCalls
import scala.util.control.TailCalls.TailRec

// FIXME: temporary value until we do proper fee calculation
val feeCoin: Coin = Coin(1_000_000L)

// TODO: Make the Address fit better into the hydrazoa type heirarchy
// (i.e., this should read InitTx instead of Transaction
class InitTxBuilder(backendService: BackendService) extends TxBuilder {
    override type Recipe = InitTxRecipe

    // Queries do a single lookup for the Tx Output of the seed utxo
    override type QueryResult = InitTxQueryResult
    override type QueryError = String
    override def query(recipe: this.Recipe): Either[QueryError, QueryResult] = {
        bloxToScalusUtxoQuery(backendService, recipe.seedUtxo) match {
            case Left(err) => Left(s"QueryError: ${err}")
            case Right(seedOutput) =>
                Right(
                  InitTxQueryResult(
                    network = recipe.network,
                    seedUtxo = recipe.seedUtxo,
                    seedOutput = seedOutput,
                    coins = recipe.coins,
                    peers = recipe.peers
                  )
                )
        }
    }

    // Calculation constructs inputs, outputs, mint values, and the native script
    override type CalculationResult = InitTxCalculationResult
    override type CalculationError = Void
    override def calculate(qr: QueryResult): Either[CalculationError, CalculationResult] = {
        // Construct head native script directly from the list of peers
        val headNativeScript = mkHeadNativeScript(qr.peers)

        /////////////////////////////
        // Construct head output

        // Put the head address of the native script
        val headAddress: Address = Shelley(
          ShelleyAddress(
            network = qr.network.toScalus,
            payment = ShelleyPaymentPart.Script(headNativeScript.scriptHash),
            delegation = Null
          )
        )

        // singleton beacon token minted by the native script with the TN being the hash of the
        // seed utxo
        // TODO: factor out "mkSingleToken"
        val beaconToken: MultiAsset = Map(
          headNativeScript.scriptHash ->
              Map(
                AssetName(
                  ByteString.fromHex(mkBeaconTokenName(qr.seedUtxo).tokenNameHex.drop(2))
                ) -> 1
              )
        )

        // Head output (L1) sits at the head address with the initial deposit from the seed utxo
        // and beacon, as well as the initial datum.
        val headValue: Value =
            Value(coin = Coin(qr.coins.toLong), multiAsset = beaconToken)
        val headOutput: TransactionOutput =
            TransactionOutput(
              address = headAddress,
              value = headValue,
              datumOption = Some(Inline(mkInitMultisigTreasuryDatum.toData))
            )

        /////////////////////
        // Change
        val changeOutput: TransactionOutput = TransactionOutput(
          address = qr.seedOutput.address,
          // Change is calculated manually here as the seed output's value, minus the
          // ada put into the head, minus the fee.
          value = qr.seedOutput.value -
              Value(Coin(qr.coins.toLong)) -
              Value(feeCoin),
          datumOption = None
        )

        Right(
          InitTxCalculationResult(
            seedUtxo = qr.seedUtxo.toScalus,
            headTO = headOutput,
            changeTO = changeOutput,
            beaconToken = beaconToken,
            headNativeScript = headNativeScript,
            headAddress = headAddress
          )
        )
    }

    override type InitializationErrorType = String
    override type InitializerResult = (Transaction, Address)
    override def txInitializer(
        cr: CalculationResult,
        entryTx: Transaction
    ): Either[InitializationErrorType, this.InitializerResult] = {
        Right(
          (
            entryTx.copy(
              body = KeepRaw(
                entryTx.body.value.copy(
                  inputs = Set(cr.seedUtxo),
                  outputs = IndexedSeq(cr.headTO, cr.changeTO).map(Sized(_)),
                  fee = feeCoin,
                  mint = Some(cr.beaconToken)
                )
              ),
              witnessSet = TransactionWitnessSet(nativeScripts = Set(cr.headNativeScript)),
              isValid = true
            ),
            cr.headAddress
          )
        )
    }

    override type FinalizerError = Void
    override type FinalizerResult = InitializerResult
    override def txFinalizer(
        cr: CalculationResult,
        initRes: InitializerResult
    ): TailRec[Either[FinalizerError, FinalizerResult]] =
        this.idFinalizer(i => i)(cr, initRes)

}

case class InitTxRecipe(
    network: Network,
    seedUtxo: UtxoIdL1,
    coins: BigInt,
    peers: Set[VerificationKeyBytes]
)

case class InitTxQueryResult(
    network: Network,
    seedUtxo: UtxoIdL1,
    seedOutput: TransactionOutput,
    coins: BigInt,
    peers: Set[VerificationKeyBytes]
)

case class InitTxCalculationResult(
    seedUtxo: TransactionInput,
    headTO: TransactionOutput,
    changeTO: TransactionOutput,
    headNativeScript: Native,
    headAddress: Address,
    beaconToken: MultiAsset
)
