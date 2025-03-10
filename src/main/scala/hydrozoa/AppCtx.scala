package hydrozoa

import com.bloxbean.cardano.client.account.Account
import com.bloxbean.cardano.client.backend.api.BackendService
import com.bloxbean.cardano.client.backend.blockfrost.service.BFBackendService
import com.bloxbean.cardano.client.common.model.Network
import scalus.builtin.ByteString
import scalus.ledger.api.v1.PubKeyHash

// TODO: AppCtx in fact just holds backendService for now

case class AppCtx(
    network: Network,
    backendService: BackendService
) {
    def pubKeyHash(accountIndex: Int) = PubKeyHash(
      ByteString.fromArray(account(accountIndex).hdKeyPair().getPublicKey.getKeyHash)
    )

    def account(accountIndex: Int) = new Account(network, AppCtx.mnemonic, accountIndex)
}

object AppCtx {
    val mnemonic =
        "test test test test test test test test test test test test test test test test test test test test test test test sauce"

    def yaciDevKit(): AppCtx = {
        val url = "http://localhost:8080/api/v1/"
        val network = new Network(0, 42)
        AppCtx(
          network,
          new BFBackendService(url, "")
        )
    }
}
