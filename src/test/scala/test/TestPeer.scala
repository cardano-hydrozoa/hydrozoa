package test

import cats.data.NonEmptyList
import com.bloxbean.cardano.client.account.Account
import com.bloxbean.cardano.client.common.model.Network as BBNetwork
import com.bloxbean.cardano.client.crypto.cip1852.DerivationPath
import com.bloxbean.cardano.client.crypto.cip1852.DerivationPath.createExternalAddressDerivationPathForAccount
import hydrozoa.*
import hydrozoa.lib.cardano.scalus.txbuilder.Transaction.attachVKeyWitnesses
import hydrozoa.lib.cardano.wallet.WalletModule
import hydrozoa.multisig.consensus.peer.{HeadPeerNumber, HeadPeerWallet}
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
    case Katie
    case Logan
    case Michael
    case Nora
    case Ophelia
    case Proteus
    case Quincy
    case Rose
    case Sarah
    case Thomas
    // Stopping here due to Yaci's limit of 20 genesis utxos.
    // case Uriel
    // case Victor
    // case Wendy
    // case Xochitl
    // case Yannis
    // case Zoe

    def account: Account = TestPeer.mkAccount(this)

    def wallet: HeadPeerWallet = TestPeer.mkWallet(this)

    def peerNum: HeadPeerNumber = TestPeer.peerNum(this)

    def address(network: Network): ShelleyAddress = TestPeer.address(this, network)

    def name: String = toString

object TestPeer:
    val nNamedPeers: Int = TestPeer.values.length
    val peerNumRange: Range = Range.Exclusive(0, nNamedPeers, 1)

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

    private val walletCache: mutable.Map[TestPeer, HeadPeerWallet] = mutable.Map.empty
        .withDefault(peer =>
            HeadPeerWallet(
              HeadPeerNumber(peer.ordinal),
              WalletModule.BloxBean,
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

    def mkWallet(peer: TestPeer): HeadPeerWallet = walletCache.useOrCreate(peer)

    def peerNum(peer: TestPeer): HeadPeerNumber = HeadPeerNumber(peer.ordinal)

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
            val keyWitness = peer.wallet.mkVKeyWitness(txUnsigned)
            txUnsigned.attachVKeyWitnesses(List(keyWitness))

    extension (wallet: HeadPeerWallet)
        def testPeerName: String = {
            val i = wallet.getPeerNum.convert
            if peerNumRange.contains(i) then TestPeer.fromOrdinal(i).toString else "Unknown"
        }

// ===================================
// Generators
// ===================================

val genTestPeer: Gen[TestPeer] = {
    for {
        i <- Gen.choose(TestPeer.peerNumRange.start, TestPeer.peerNumRange.last)
    } yield TestPeer.fromOrdinal(i)
}

def genTestPeers(minPeers: Int = 2, maxPeers: Int = 5): Gen[NonEmptyList[TestPeer]] = {
    require(0 < minPeers && minPeers < TestPeer.nNamedPeers)
    require(minPeers <= maxPeers && maxPeers < TestPeer.nNamedPeers)
    for {
        numPeers <- Gen.choose(minPeers, maxPeers)
        peers = TestPeer.peerNumRange.take(numPeers).map(TestPeer.fromOrdinal)
    } yield NonEmptyList.fromListUnsafe(peers.toList)
}
