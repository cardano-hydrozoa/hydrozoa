package hydrozoa.deploy

import com.bloxbean.cardano.client.api.model.Amount
import com.bloxbean.cardano.client.backend.api.BackendService
import com.bloxbean.cardano.client.function.helper.SignerProviders
import com.bloxbean.cardano.client.quicktx.{QuickTxBuilder, Tx}
import com.bloxbean.cardano.client.spec.Script
import hydrozoa.TxL1
import hydrozoa.infra.Piper
import hydrozoa.node.TestPeer

def mkDeployTx(backendService: BackendService, peer: TestPeer, script: Script): TxL1 =

    val account = TestPeer.account(peer)
    val address = account.baseAddress()

    val txPartial = Tx()
        .payToAddress(
          address,
          Amount.ada(100),
          script
        )
        .from(address)

    val builder = QuickTxBuilder(backendService)

    builder
        .compose(txPartial)
        .feePayer(address)
        .withSigner(SignerProviders.signerFrom(account))
        .buildAndSign
        .serialize()
        |> TxL1.apply

end mkDeployTx
