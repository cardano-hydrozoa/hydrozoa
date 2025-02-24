package hydrozoa.head.wallet

import com.bloxbean.cardano.client.crypto.bip32.HdKeyPair
import hydrozoa.head.l1.AppCtx
import hydrozoa.head.{L1Tx, TxKeyWitness, signTxWallet}

class MockWallet(ctx: AppCtx) extends Wallet {
    override def sign(tx: L1Tx): TxKeyWitness = {
        val pair: HdKeyPair = ctx.account.hdKeyPair
        signTxWallet(tx, pair)
    }
}
