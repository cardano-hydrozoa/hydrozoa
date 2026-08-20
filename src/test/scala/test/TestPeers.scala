package test

import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyList, NonEmptyMap, ReaderT}
import com.bloxbean.cardano.client.account.Account
import com.bloxbean.cardano.client.common.model.Network as BloxbeanNetwork
import com.bloxbean.cardano.client.crypto.cip1852.DerivationPath.createExternalAddressDerivationPathForAccount
import hydrozoa.*
import hydrozoa.config.head.coil.{CoilPeerData, CoilPeers}
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.network.CardanoNetworkGen.given_Arbitrary_CardanoNetwork
import hydrozoa.config.head.peers.{HeadPeerData, HeadPeers}
import hydrozoa.lib.cardano.scalus.VerificationKeyExtra.shelleyAddress
import hydrozoa.lib.cardano.scalus.txbuilder.Transaction.attachVKeyWitnesses
import hydrozoa.lib.cardano.wallet.WalletModule
import hydrozoa.multisig.consensus.peer.{CoilPeerNumber, HeadPeerId, HeadPeerNumber, PeerWallet}
import hydrozoa.multisig.ledger.l1.tx.EnrichedTx
import org.bouncycastle.crypto.params.Ed25519PrivateKeyParameters
import org.http4s.Uri
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Test.Parameters
import org.scalacheck.{Gen, Prop, Properties}
import scala.collection.concurrent.TrieMap
import scala.collection.immutable.SortedMap
import scala.collection.mutable
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.{Transaction, VKeyWitness}
import scalus.crypto.ed25519.{SigningKey, VerificationKey}
import scalus.uplc.builtin.{Builtins, ByteString}
import scalus.|>

type GenWithTestPeers[A] = ReaderT[Gen, TestPeers, A]

/** TestPeers object provides everything test suites may need to operate a peer in a head:
  *   - head peer numbers
  *   - head peer ids
  *   - head peer verification keys
  *   - head peer addresses
  *   - head peer wallets
  *
  * There is a good reason why this thing exists separately from the head config / node config /
  * multi-node config: integration tests use it when setting up the environment (see
  * ModelBasedSuite.Env). This happens before the initial state is built, but we need to run some
  * transactions on behalf of prospective head peers.
  */

case class TestPeers private (
    seedPhrase: SeedPhrase,
    override val cardanoNetwork: CardanoNetwork,
    peersNumber: Int,
    coilPeersNumber: Int,
    keyScheme: TestPeers.KeyScheme,
) extends CardanoNetwork.Section,
      HeadPeers.Section {
    import TestPeerName.maxPeers

    private val peerNumbers: List[Int] = List.range(0, peersNumber)

    // Head peers occupy ordinals `[0, peersNumber)`; coil peers occupy
    // `[peersNumber, peersNumber + coilPeersNumber)`.
    private def _require(peer: TestPeerName): Unit =
        require(
          peer.ordinal < peersNumber + coilPeersNumber,
          s"Can't access peer $peer; head=$peersNumber, coil=$coilPeersNumber"
        )

    require(
      peersNumber <= maxPeers,
      s"The number of peers are limited to $maxPeers "
    )
    require(
      coilPeersNumber >= 0 && peersNumber + coilPeersNumber <= maxPeers,
      s"Coil peers ($coilPeersNumber) + head peers ($peersNumber) must fit in $maxPeers"
    )

    // ===================================
    // API
    // ===================================

    // A `lazy val`, not a `def`: [[HeadPeers.Section]] derives headPeerNums / headPeerIds /
    // headPeerVKeys / nHeadPeers from this, and generators call those per sample — recomputing the
    // peer set each time meant re-deriving every peer's key (see [[verificationKeyFor]]).
    override lazy val headPeers: HeadPeers = {
        def helper[A](f: TestPeerName => A) =
            NonEmptyList.fromListUnsafe(
              peerNumbers.map(ix => f(TestPeerName.fromOrdinal(ix)))
            )

        val headPeerVKeys: NonEmptyList[VerificationKey] = helper(verificationKeyFor)

        val headPeersAddresses: NonEmptyList[Uri] = helper(webSocketAddressFor)

        headPeerVKeys
            .zip(headPeersAddresses)
            .map(HeadPeerData(_, _))
            .zipWithIndex
            .map(_.swap)
            .map((idx, data) => (HeadPeerNumber(idx), data))
            .toList
            |> SortedMap.from
            |> NonEmptyMap.fromMapUnsafe
            |> HeadPeers.apply
            |> (x => x.get)

    }

    def webSocketAddressFor(peerNumber: HeadPeerNumber): Uri =
        webSocketAddressFor(TestPeerName.fromOrdinal(peerNumber))

    // TODO: What do we want here?
    def webSocketAddressFor(peer: TestPeerName): Uri = {
        _require(peer)
        // Port 0 → the OS assigns a free ephemeral port when a head node binds its mesh server from
        // this advertised address (the bind source after the merge). Tests that dial reconstruct
        // URIs from the actually-bound port, so the placeholder port here is never dialed.
        Uri.unsafeFromString(s"ws://localhost:0/${peer.name}")
    }

    def verificationKeyFor(peerNumber: HeadPeerNumber): VerificationKey =
        verificationKeyFor(TestPeerName.fromOrdinal(peerNumber))

    def verificationKeyFor(peer: TestPeerName): VerificationKey =
        _require(peer)
        vkeyFor(peer)

    def shelleyAddressFor(peerNumber: HeadPeerNumber): ShelleyAddress =
        shelleyAddressFor(TestPeerName.fromOrdinal(peerNumber))

    def shelleyAddressFor(peer: TestPeerName): ShelleyAddress = {
        _require(peer)
        addressCache.useOrCreate(peer)
    }

    def walletFor(peerNumber: HeadPeerNumber): PeerWallet =
        walletFor(TestPeerName.fromOrdinal(peerNumber))

    def walletFor(peer: TestPeerName): PeerWallet =
        require(
          peer.ordinal < peersNumber,
          s"Can't access peer $peer there is only $peersNumber is the head"
        )
        walletFor_(peer)

    /** Coil peer wallet by [[CoilPeerNumber]]. Coil peers occupy ordinals
      * `[peersNumber, peersNumber + coilPeersNumber)` in the same seed-derived space as the head
      * peers, so their vkeys are stable per (seed, coil-index) — the head bootstrap can pin them in
      * `coilPeers` and the coil-side node config can pick them up by index.
      */
    def coilWalletFor(n: CoilPeerNumber): PeerWallet =
        require(
          n.convert < coilPeersNumber,
          s"Can't access coil peer $n; only $coilPeersNumber coil peer(s) configured"
        )
        walletFor_(TestPeerName.fromOrdinal(peersNumber + n.convert))

    /** Every coil wallet in [[CoilPeerNumber]] order — the same order they appear in the
      * [[CoilPeers]] config built by [[coilPeersConfig]].
      */
    def coilWallets: List[PeerWallet] =
        (0 until coilPeersNumber).toList.map(i => coilWalletFor(CoilPeerNumber(i)))

    /** Head-bootstrap [[CoilPeers]] config with every coil peer hubbed by `hub`. Convenient
      * shorthand for the common "one hub for everyone" test topology; multi-hub setups can build
      * the [[CoilPeers]] value directly.
      */
    def coilPeersConfig(hub: HeadPeerNumber): CoilPeers =
        CoilPeers.indexed(
          coilWallets.map(w => CoilPeerData(w.exportVerificationKey, hub))
        )

    /** This is needed here to sign the initialization tx, when we still don't have
      * [[MultiNodeConfig]].
      */
    def multisignTx(tx: Transaction): Transaction =
        tx.attachVKeyWitnesses(mkVKeyWitnesses(tx).toList)

    def multisignTx[A <: EnrichedTx[A]](tx: A): A =
        val witnesses = mkVKeyWitnesses(tx.tx)
        tx.addSignatures(Set.from(witnesses.toList)) match {
            case Valid(a) =>
                a
            case Invalid(e) => throw RuntimeException(s"error multi-signing: $e")
        }

    def mkVKeyWitnesses(tx: Transaction): NonEmptyList[VKeyWitness] =
        NonEmptyList.fromListUnsafe(
          peerNumbers.map(n => walletFor(HeadPeerNumber(n)).mkVKeyWitness(tx))
        )

    // ===================================
    // Internal
    // ==================================

    extension [K, V](map: mutable.Map[K, V])
        def useOrCreate(key: K): V = map.get(key) match {
            case None =>
                val missing = map.default(key)
                @annotation.unused
                val _ = map.put(key, missing)
                missing
            case Some(value) => value
        }

    private val accountCache: mutable.Map[TestPeerName, Account] = mutable.Map.empty
        .withDefault(peer =>
            Account.createFromMnemonic(
              cardanoNetwork.asBloxbeanNetwork,
              seedPhrase.mnemonic,
              createExternalAddressDerivationPathForAccount(peer.ordinal)
            )
        )

    private def bloxbeanAccountFor(peer: TestPeerName): Account = accountCache.useOrCreate(peer)

    /** Caching the [[Account]] is not enough: Bloxbean re-runs the whole derivation on every key
      * access (`getHdKeyPairFromDerivationPath` → `getRootKeyPairFromMnemonic` →
      * `pbkdf2HmacSha512`), which dominates the suite's runtime once a generator reaches a peer's
      * key per sample. The derived key and wallet are therefore cached in the companion, not here —
      * see [[TestPeers.vkeyCache]] for why instance scope is not enough.
      */
    private def vkeyFor(peer: TestPeerName): VerificationKey =
        TestPeers.vkeyCache.getOrElseUpdate(
          (seedPhrase.mnemonic, peer.ordinal, keyScheme),
          keyScheme match {
              case TestPeers.KeyScheme.Bip32 =>
                  VerificationKey.unsafeFromArray(bloxbeanAccountFor(peer).publicKeyBytes())
              case TestPeers.KeyScheme.Ed25519 =>
                  TestPeers.ed25519KeyPair(seedPhrase, peer)._1
          }
        )

    private def walletFor_(peer: TestPeerName): PeerWallet =
        TestPeers.walletCache.getOrElseUpdate(
          (seedPhrase.mnemonic, peer.ordinal, keyScheme),
          keyScheme match {
              case TestPeers.KeyScheme.Bip32 =>
                  val hdKeyPair = bloxbeanAccountFor(peer).hdKeyPair()
                  PeerWallet(
                    WalletModule.BloxBean,
                    hdKeyPair.getPublicKey,
                    hdKeyPair.getPrivateKey
                  )
              case TestPeers.KeyScheme.Ed25519 =>
                  val (vkey, skey) = TestPeers.ed25519KeyPair(seedPhrase, peer)
                  PeerWallet.scalusWallet(vkey, skey)
          }
        )

    // Stays instance-scoped: unlike the key it is derived from, an address *is* network-specific.
    private val addressCache: mutable.Map[TestPeerName, ShelleyAddress] =
        mutable.Map.empty.withDefault(peer =>
            verificationKeyFor(peer).shelleyAddress()(using cardanoNetwork)
        )

}

object TestPeers:

    /** Derived peer keys, shared across every [[TestPeers]] instance in the JVM.
      *
      * It has to outlive the instance: `TestPeers.arbitrary` builds a fresh one per ScalaCheck
      * sample, so an instance-scoped cache would still pay a full BIP32 derivation per peer per
      * sample — the suite's dominant cost. Keyed by `(mnemonic, ordinal)` and **not** by network,
      * because BIP32 derivation does not involve the network; only address encoding does, and that
      * happens downstream of the key. `TestPeersTest` pins that. The key space is therefore tiny:
      * the few seed phrases in use times at most `TestPeerName.maxPeers`.
      *
      * Concurrent by construction — suites run in parallel and share this map.
      */
    private val vkeyCache: TrieMap[(String, Int, KeyScheme), VerificationKey] = TrieMap.empty

    /** Peer wallets, shared for the same reason as [[vkeyCache]] and keyed identically: the HD key
      * pair behind a wallet comes from the same network-independent derivation. Safe to share —
      * [[PeerWallet]] is an immutable holder of a key pair whose methods are pure.
      */
    private val walletCache: TrieMap[(String, Int, KeyScheme), PeerWallet] = TrieMap.empty

    def arbitrary: Gen[TestPeers] = for {
        spec <- TestPeersSpec.generate()
        testPeers <- generate(spec)
    } yield testPeers

    /** Which key type a peer's wallet holds.
      *
      * `Bip32` is the default: Cardano's Ed25519-BIP32 extended keys, via BloxBean. A config
      * written from one cannot boot a node
      * ([[hydrozoa.multisig.consensus.peer.PeerWallet.peerWalletEncoder]]).
      *
      * `Ed25519` derives a plain 32-byte key, which round-trips through JSON intact. Choose it when
      * a test writes a config to disk and then runs a node from the file.
      */
    enum KeyScheme derives CanEqual:
        case Bip32, Ed25519

    /** Deterministic plain-ed25519 key pair for a peer, from the same `(mnemonic, ordinal)` the
      * BIP32 path keys on. Not a BIP32 derivation and not interchangeable with one: it exists only
      * so a generated config survives a JSON round trip.
      */
    private[test] def ed25519KeyPair(
        seedPhrase: SeedPhrase,
        peer: TestPeerName
    ): (VerificationKey, SigningKey) = {
        val seed = Builtins.blake2b_256(
          ByteString.fromString(s"${seedPhrase.mnemonic}#${peer.ordinal}")
        )
        val sk = Ed25519PrivateKeyParameters(seed.bytes, 0)
        (
          VerificationKey.unsafeFromArray(sk.generatePublicKey().getEncoded),
          SigningKey.unsafeFromByteString(ByteString.fromArray(sk.getEncoded))
        )
    }

    def apply(
        seedPhrase: SeedPhrase,
        network: CardanoNetwork,
        peersNumber: Int,
        coilPeersNumber: Int = 0,
        keyScheme: KeyScheme = KeyScheme.Bip32,
    ): TestPeers =
        new TestPeers(seedPhrase, network, peersNumber, coilPeersNumber, keyScheme)

    def generate(spec: TestPeersSpec): Gen[TestPeers] =
        import TestPeerName.maxPeers

        for {
            peersNumber <- spec.peersNumberSpec match {
                case PeersNumberSpec.Random =>
                    Gen.choose(1, maxPeers)
                case PeersNumberSpec.Range(mbMin, mbMax) =>
                    val min = mbMin.getOrElse(1)
                    val max = mbMax.getOrElse(maxPeers)
                    require(0 < min && min <= max && max <= maxPeers)
                    Gen.choose(min, max)
                case PeersNumberSpec.Exact(peersNumber) =>
                    require(0 < peersNumber && peersNumber <= maxPeers)
                    Gen.const(peersNumber)
            }
        } yield TestPeers(spec.seedPhrase, spec.network, peersNumber)

/** Test head peer names are just better indexes - so you can have Alice in one-peer head, Alice and
  * Bob in two-peer head and so on - indexe/name correspondence is static, you won't see Alice
  * appears under any other peer number but 0. Try not to overuse it in the code that is not the
  * test scenatios/utils.
  */
enum TestPeerName derives CanEqual:
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

    def headPeerNumber: HeadPeerNumber = HeadPeerNumber(this.ordinal)
    def name: String = toString

object TestPeerName:
    def apply(headPeerNumber: HeadPeerNumber): TestPeerName = {
        assert(headPeerNumber < TestPeerName.values.length)
        TestPeerName.fromOrdinal(headPeerNumber)
    }

    def apply(headPeerId: HeadPeerId): TestPeerName = apply(headPeerId._1)

    val maxPeers: Int = TestPeerName.values.length

case class TestPeersSpec(
    seedPhrase: SeedPhrase,
    network: CardanoNetwork,
    peersNumberSpec: PeersNumberSpec
) {
    def withPeersNumberSpec(spec: PeersNumberSpec): TestPeersSpec =
        this.copy(peersNumberSpec = spec)
}

object TestPeersSpec:

    def default: TestPeersSpec =
        TestPeersSpec(
          SeedPhrase.Yaci,
          CardanoNetwork.Preprod,
          PeersNumberSpec.Range(Some(2), Some(5))
        )

    def generate(): Gen[TestPeersSpec] =
        for {
            seedPhrase <- Gen.oneOf(SeedPhrase.Yaci, SeedPhrase.Public)
            network <- arbitrary[CardanoNetwork]
            peersNumberSpec <- PeersNumberSpec.generate()
        } yield TestPeersSpec(seedPhrase, network, peersNumberSpec)

extension (self: CardanoNetwork)
    def asBloxbeanNetwork: BloxbeanNetwork =

        BloxbeanNetwork(self.cardanoInfo.network.networkId.toInt, self.protocolMagic)

enum PeersNumberSpec:
    case Random
    case Range(mbMin: Option[Int] = None, mbMax: Option[Int] = None)
    case Exact(peersNumber: Int)

object PeersNumberSpec:

    def generate(): Gen[PeersNumberSpec] = Gen.oneOf(
      Gen.const(Random),
      Gen.choose(3, 5).map { minPeers =>
          Range(mbMin = Some(minPeers))
      },
      Gen.choose(7, 10).map { maxPeers =>
          Range(mbMax = Some(maxPeers))
      },
      Gen.choose(3, 5).flatMap { minPeers =>
          Gen.choose(minPeers + 1, 10)
              .map(maxPeers => Range(mbMin = Some(minPeers), mbMax = Some(maxPeers)))
      },
      Gen.const(Exact(1)),
      Gen.const(Exact(2)),
      Gen.const(Exact(TestPeerName.maxPeers))
    )

object TestPeersTest extends Properties("Test peers") {
    override def overrideParameters(p: Parameters): Parameters =
        p.withMinSuccessfulTests(500)

    val _ = property("generates") = Prop.forAll(
      TestPeersSpec
          .generate()
          .flatMap(TestPeers.generate)
    )(testPeers => Prop.collect(testPeers)(Prop.passed))

    /** [[TestPeers.vkeyCache]] and [[TestPeers.walletCache]] are keyed by `(mnemonic, ordinal)`
      * with no network component, which is only sound because BIP32 derivation does not involve the
      * network — only address encoding does, downstream of the key. Pin that: the same seed and
      * ordinal must yield both the same verification key and an equivalently-signing wallet on any
      * two networks.
      */
    val _ = property("a peer's key and wallet do not depend on the network") = Prop.forAll(
      Gen.oneOf(SeedPhrase.Yaci, SeedPhrase.Public),
      arbitrary[CardanoNetwork],
      arbitrary[CardanoNetwork],
      Gen.choose(0, TestPeerName.maxPeers - 1)
    ) { (seedPhrase, networkA, networkB, ordinal) =>
        // Derive straight through Bloxbean rather than via TestPeers: going through the caches the
        // property exists to justify would make it vacuously true.
        def accountOn(network: CardanoNetwork): Account =
            Account.createFromMnemonic(
              network.asBloxbeanNetwork,
              seedPhrase.mnemonic,
              createExternalAddressDerivationPathForAccount(ordinal)
            )
        def walletOf(account: Account): PeerWallet =
            val hdKeyPair = account.hdKeyPair()
            PeerWallet(WalletModule.BloxBean, hdKeyPair.getPublicKey, hdKeyPair.getPrivateKey)

        val accountA = accountOn(networkA)
        val accountB = accountOn(networkB)
        // PeerWallet's equality is extensional — same exported vkey *and* same signature over a
        // fixed message — so this covers the signing key, not just the public one.
        accountA.publicKeyBytes().toList == accountB.publicKeyBytes().toList &&
        walletOf(accountA) == walletOf(accountB)
    }
}
