package hydrozoa.l1.wallet

import com.bloxbean.cardano.client.crypto.bip32.HdKeyPair
import hydrozoa.infra
import hydrozoa.{AppCtx, L1Tx, TxKeyWitness}

class MockWallet(ctx: AppCtx, accountIndex: Int) extends Wallet {
    override def createTxKeyWitness(tx: L1Tx): TxKeyWitness = {
        val pair: HdKeyPair = ctx.account(accountIndex).hdKeyPair
        infra.createTxKeyWitness(tx, pair)
    }
}
