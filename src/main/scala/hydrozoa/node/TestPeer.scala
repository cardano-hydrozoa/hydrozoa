package hydrozoa.node

import com.bloxbean.cardano.client.account.Account
import com.bloxbean.cardano.client.crypto.cip1852.DerivationPath
import com.bloxbean.cardano.client.crypto.cip1852.DerivationPath.createExternalAddressDerivationPathForAccount
import hydrozoa.infra.{WalletModuleBloxbean, toBloxbean}
import hydrozoa.node.state.WalletId
import hydrozoa.{Wallet, networkL1static}

import scala.collection.mutable

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

    def compareTo(another: TestPeer): Int = this.toString.compareTo(another.toString)

object TestPeer:
    private val mnemonic: String =
        "test test test test " +
            "test test test test " +
            "test test test test " +
            "test test test test " +
            "test test test test " +
            "test test test sauce"

    private val accountCache: mutable.Map[TestPeer, Account] = mutable.Map.empty
        .withDefault(peer =>
            new Account(
              networkL1static.toBloxbean,
              mnemonic,
              createExternalAddressDerivationPathForAccount(peer.ordinal)
            )
        )

    private val walletCache: mutable.Map[TestPeer, Wallet] = mutable.Map.empty
        .withDefault(peer =>
            Wallet(
              peer.toString,
              WalletModuleBloxbean,
              account(peer).hdKeyPair().getPublicKey,
              account(peer).hdKeyPair().getPrivateKey
            )
        )

    def account(peer: TestPeer): Account = accountCache.cache(peer)

    def mkWallet(peer: TestPeer): Wallet = walletCache.cache(peer)

    def mkWalletId(peer: TestPeer): WalletId = WalletId(peer.toString)

extension [K, V](map: mutable.Map[K, V])
    def cache(key: K): V = map.get(key) match {
        case None =>
            val missing = map.default(key)
            map.put(key, missing)
            missing
        case Some(value) => value
    }
