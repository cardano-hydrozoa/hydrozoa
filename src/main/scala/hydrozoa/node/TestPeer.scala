package hydrozoa.node

import com.bloxbean.cardano.client.account.Account
import com.bloxbean.cardano.client.crypto.cip1852.DerivationPath
import com.bloxbean.cardano.client.crypto.cip1852.DerivationPath.createExternalAddressDerivationPathForAccount
import hydrozoa.infra.{WalletModuleBloxbean, toBloxbean}
import hydrozoa.node.state.WalletId
import hydrozoa.{Wallet, networkL1static}

enum TestPeer(ix: Int) derives CanEqual:
    case Alice extends TestPeer(0)
    case Bob extends TestPeer(1)
    case Carol extends TestPeer(2)
    case Daniella extends TestPeer(3)
    case Erin extends TestPeer(4)
    case Frank extends TestPeer(5)
    case Gustavo extends TestPeer(6)
    case Hector extends TestPeer(7)
    case Isabel extends TestPeer(8)
    case Julia extends TestPeer(9)

object TestPeer:
    val mnemonic: String =
        "test test test test " +
            "test test test test " +
            "test test test test " +
            "test test test test " +
            "test test test test " +
            "test test test sauce"

    def account(peer: TestPeer) =
        new Account(
          networkL1static.toBloxbean,
          mnemonic,
          createExternalAddressDerivationPathForAccount(peer.ordinal)
        )

    def mkWallet(peer: TestPeer): Wallet =
        Wallet(
          peer.toString,
          WalletModuleBloxbean,
          account(peer).hdKeyPair().getPublicKey,
          account(peer).hdKeyPair().getPrivateKey
        )

    def mkPeerInfo(peer: TestPeer): WalletId =
        WalletId(peer.toString)
