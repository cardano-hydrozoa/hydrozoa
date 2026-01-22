package test

import cats.data.NonEmptyList
import com.bloxbean.cardano.client.account.Account
import com.bloxbean.cardano.client.common.model.Network as BBNetwork
import com.bloxbean.cardano.client.crypto.cip1852.DerivationPath
import com.bloxbean.cardano.client.crypto.cip1852.DerivationPath.createExternalAddressDerivationPathForAccount
import hydrozoa.*
import org.scalacheck.Gen
import scala.collection.mutable
import scalus.builtin.Builtins.blake2b_224
import scalus.builtin.ByteString
import scalus.cardano.address.ShelleyDelegationPart.Null
import scalus.cardano.address.ShelleyPaymentPart.Key
import scalus.cardano.address.{Network, ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.ledger.ArbitraryInstances.*
import scalus.cardano.ledger.{Hash, Transaction as STransaction}

enum TestPeer derives CanEqual:
    case Alice
    case Bob
    case Carol
    case Daniella
    case Erin
    case Frank
    case Gustavo
    case Hector
    case Isabel
    case Julia

    // def compareTo(another: TestPeer): Int = this.ordinal.compareTo(another.ordinal)

    def account: Account = TestPeer.mkAccount(this)

    def wallet: Wallet = TestPeer.mkWallet(this)

    def walletId: WalletId = TestPeer.mkWalletId(this)

    def address(network: Network = testNetwork): ShelleyAddress = TestPeer.address(this, network)

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
            Account.createFromMnemonic(
              BBNetwork(0, 42),
              mnemonic,
              createExternalAddressDerivationPathForAccount(peer.ordinal)
            )
        )

    private val walletCache: mutable.Map[TestPeer, Wallet] = mutable.Map.empty
        .withDefault(peer =>
            Wallet(
              peer.toString,
              WalletModuleBloxbean,
              mkAccount(peer).hdKeyPair().getPublicKey,
              mkAccount(peer).hdKeyPair().getPrivateKey
            )
        )

    private val addressCache: mutable.Map[TestPeer, (ShelleyPaymentPart, ShelleyDelegationPart)] =
        mutable.Map.empty.withDefault(peer =>
            (
              Key(Hash(blake2b_224(ByteString.fromArray(mkAccount(peer).publicKeyBytes())))),
              Null
            )
        )

    def mkAccount(peer: TestPeer): Account = accountCache.useOrCreate(peer)

    def mkWallet(peer: TestPeer): Wallet = walletCache.useOrCreate(peer)

    def mkWalletId(peer: TestPeer): WalletId = WalletId(peer.toString)

    def address(network: Network)(peer: TestPeer): ShelleyAddress = address(peer, network)

    def address(peer: TestPeer, network: Network): ShelleyAddress = {
        val (payment, delegation) = addressCache.useOrCreate(peer)
        ShelleyAddress(network, payment, delegation)
    }

    extension [K, V](map: mutable.Map[K, V])
        def useOrCreate(key: K): V = map.get(key) match {
            case None =>
                val missing = map.default(key)
                @annotation.unused
                val _ = map.put(key, missing)
                missing
            case Some(value) => value
        }

    extension (peer: TestPeer)
        def signTx(txUnsigned: STransaction): STransaction =
            val keyWitness = peer.wallet.createTxKeyWitness(txUnsigned)
            attachVKeyWitnesses(txUnsigned, List(keyWitness))

// ===================================
// Generators
// ===================================

val genTestPeer: Gen[TestPeer] =
    Gen.oneOf(
      TestPeer.Alice,
      TestPeer.Bob,
      TestPeer.Carol,
      TestPeer.Daniella,
      TestPeer.Erin,
      TestPeer.Frank,
      TestPeer.Gustavo,
      TestPeer.Hector,
      TestPeer.Isabel,
      TestPeer.Julia
    )

/** Choose between 2 and 10 peers */
def genTestPeers(minPeers: Int = 2): Gen[NonEmptyList[TestPeer]] =
    for {
        numPeers <- Gen.choose(minPeers, 10)
        peersList = List(
          TestPeer.Alice,
          TestPeer.Bob,
          TestPeer.Carol,
          TestPeer.Daniella,
          TestPeer.Erin,
          TestPeer.Frank,
          TestPeer.Gustavo,
          TestPeer.Hector,
          TestPeer.Isabel,
          TestPeer.Julia
        )
    } yield NonEmptyList.fromList(peersList.take(numPeers)).get
