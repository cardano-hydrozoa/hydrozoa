package hydrozoa.head.l1

import com.bloxbean.cardano.client.address.AddressProvider.getEntAddress
import com.bloxbean.cardano.client.api.model.Amount.{ada, asset}
import com.bloxbean.cardano.client.api.model.{ProtocolParams, Result}
import com.bloxbean.cardano.client.backend.api.DefaultUtxoSupplier
import com.bloxbean.cardano.client.crypto.{SecretKey, VerificationKey}
import com.bloxbean.cardano.client.function.TxSigner
import com.bloxbean.cardano.client.function.helper.SignerProviders
import com.bloxbean.cardano.client.plutus.spec.PlutusData
import com.bloxbean.cardano.client.quicktx.{QuickTxBuilder, Tx}
import com.bloxbean.cardano.client.transaction.spec.script.NativeScript
import com.bloxbean.cardano.client.transaction.spec.{Asset, Transaction}
import com.bloxbean.cardano.client.util.HexUtil.encodeHexString
import hydrozoa.head.multisig.assetNamePrefix
import hydrozoa.head.{AppCtx, HeadParams, Tx_, UtxoRef}
import scalus.bloxbean.{EvaluatorMode, NoScriptSupplier, ScalusTransactionEvaluator, SlotConfig}

import java.math.BigInteger
import scala.jdk.CollectionConverters.*

/**
 * @param params head params
 * @param seed   seed output
 * @return serialized transaction
 */
def mkInitTx(params: HeadParams, seed: UtxoRef): Tx_ = {
  // TODO: implement
  Tx_(Array.empty[Byte])
}

extension [A](result: Result[A])
  def toEither: Either[String, A] =
    if result.isSuccessful then Right(result.getValue)
    else Left(result.getResponse)

class TxBuilder(ctx: AppCtx) {
  lazy val protocolParams: ProtocolParams = {
    val result = backendService.getEpochService.getProtocolParameters
    if !result.isSuccessful then sys.error(result.getResponse)
    result.getValue
  }
  private lazy val quickTxBuilder = QuickTxBuilder(backendService)
  private lazy val utxoSupplier = new DefaultUtxoSupplier(backendService.getUtxoService)
  private lazy val evaluator = ScalusTransactionEvaluator(
    slotConfig = SlotConfig.Preprod,
    protocolParams = protocolParams,
    utxoSupplier = utxoSupplier,
    scriptSupplier = NoScriptSupplier(),
    mode = EvaluatorMode.EVALUATE_AND_COMPUTE_COST
  )
  private val backendService = ctx.backendService
  private val account = ctx.account

  def submitInitTx(
                    amount: Long,
                    nativeScript: NativeScript,
                    vKeys: Set[VerificationKey],
                    sKeys: Set[SecretKey],
                  ): Either[String, String] = {
    val signedTxE = mkInitTx(amount, nativeScript, vKeys, sKeys)
    println("Initializaton tx: " + signedTxE.map(t => t.serializeToHex()).merge)
    for
      signedTx <- signedTxE
      result = backendService.getTransactionService.submitTransaction(signedTx.serialize())
      r <- Either.cond(result.isSuccessful, result.getValue, result.getResponse)
    yield r
  }

  def mkInitTx(
                amount: Long,
                nativeScript: NativeScript,
                vKeys: Set[VerificationKey],
                sKeys: Set[SecretKey]
              ): Either[String, Transaction] = {
    val wallet = account.getBaseAddress.getAddress

    for
      utxo <- backendService.getUtxoService.getUtxos(wallet, 100, 1).toEither

      headAddress = getEntAddress(nativeScript, ctx.network).toBech32

      beaconToken = Asset.builder
        .name(encodeHexString(assetNamePrefix ++ "HeadTokenName".getBytes, true))
        .value(BigInteger.valueOf(1))
        .build

      scriptTx = new Tx()
        .from(wallet) // otherwise it throws "No sender address or sender account defined"
        .mintAssets(nativeScript, beaconToken, headAddress)
        .collectFrom(utxo)
        .withChangeAddress(wallet)
        .payToContract(
          headAddress,
          List(
            ada(amount),
            //                asset(nativeScript.getPolicyId, beaconToken.getName, BigInteger.valueOf(1))
          ).asJava,
          PlutusData.unit
        )


      signers: List[TxSigner] = sKeys.map(k => SignerProviders.signerFrom(k)).toList

      signedTx = quickTxBuilder
        .compose(scriptTx)
        // evaluate script cost using scalus
        .withTxEvaluator(evaluator)
        //
        .withSigner(SignerProviders.signerFrom(account))
        .withSigner(SignerProviders.signerFrom(sKeys.toList(0)))
        .withSigner(SignerProviders.signerFrom(sKeys.toList(1)))
        .withSigner(SignerProviders.signerFrom(sKeys.toList(2)))
        //
        .withRequiredSigners(account.getBaseAddress)
        .feePayer(account.baseAddress())
        .buildAndSign()
    yield signedTx
  }
}
