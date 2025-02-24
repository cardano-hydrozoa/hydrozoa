package hydrozoa.l1.wallet

import com.bloxbean.cardano.client.crypto.bip32.HdKeyPair
import hydrozoa.infra.signTxWallet
import hydrozoa.{AppCtx, L1Tx, TxKeyWitness}

class MockWallet(ctx: AppCtx) extends Wallet {
    override def sign(tx: L1Tx): TxKeyWitness = {
        val pair: HdKeyPair = ctx.account.hdKeyPair
        signTxWallet(tx, pair)
    }
}
