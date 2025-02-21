package hydrozoa.head

import com.bloxbean.cardano.client.account.Account
import com.bloxbean.cardano.client.backend.api.BackendService
import com.bloxbean.cardano.client.backend.blockfrost.service.BFBackendService
import com.bloxbean.cardano.client.common.model.Network
import com.bloxbean.cardano.client.crypto.{KeyGenUtil, Keys}
import hydrozoa.head.l1.TxBuilder
import hydrozoa.head.multisig.mkHeadNativeScript
import hydrozoa.head.network.HydrozoaNetwork
import hydrozoa.logging.LoggingService
import scalus.builtin.ByteString
import scalus.ledger.api.v1.PubKeyHash

// TODO: move to a "clean module" that doesn't depend on the libs

class Node(
            ctx: AppCtx,
            network: HydrozoaNetwork,
            logging: LoggingService
          ):

  private val ownKeys: Keys = KeyGenUtil.generateKey
  private val txBuilder = TxBuilder(ctx)

  def initializeHead(amount: Long): Either[String, String] = {
    // FIXME: check head/node status
    logging.logInfo("Trying to initialize the head with initial treasury of " + amount + "ADA.")

    val vKeys = network.participantsKeys() + ownKeys.getVkey
    val headNativeScript = mkHeadNativeScript(vKeys)
    val policyId = headNativeScript.getPolicyId

    logging.logInfo("Native script policy id: " + policyId)

    val sKeys = network.participantsSigningKeys() + ownKeys.getSkey
    
    for {
      ret <- txBuilder.submitInitTx(amount, headNativeScript, vKeys, sKeys)
    } yield ret
  }

// TODO: AppCtx in fact just holds backendService for now

case class AppCtx(
                   network: Network,
                   account: Account,
                   backendService: BackendService,
                 ) {
  lazy val pubKeyHash: PubKeyHash = PubKeyHash(
    ByteString.fromArray(account.hdKeyPair().getPublicKey.getKeyHash)
  )
}


object AppCtx {

  def yaciDevKit(): AppCtx = {
    val url = "http://localhost:8080/api/v1/"
    val network = new Network(0, 42)
    val mnemonic =
      "test test test test test test test test test test test test test test test test test test test test test test test sauce"
    AppCtx(
      network,
      new Account(network, mnemonic),
      new BFBackendService(url, ""),
    )
  }
}
