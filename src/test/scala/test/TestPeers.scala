package test

import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyList, NonEmptyMap, ReaderT}
import hydrozoa.*
import hydrozoa.config.head.coil.{CoilPeerData, CoilPeers}
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.network.CardanoNetworkGen.given_Arbitrary_CardanoNetwork
import hydrozoa.config.head.peers.{HeadPeerData, HeadPeers}
import hydrozoa.lib.cardano.scalus.VerificationKeyExtra.shelleyAddress
import hydrozoa.lib.cardano.scalus.txbuilder.Transaction.attachVKeyWitnesses
import hydrozoa.multisig.consensus.peer.{CoilPeerNumber, HeadPeerId, HeadPeerNumber, PeerWallet}
import hydrozoa.multisig.ledger.l1.tx.EnrichedTx
import java.nio.charset.StandardCharsets
import java.security.MessageDigest
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
import scalus.uplc.builtin.ByteString
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

    private def vkeyFor(peer: TestPeerName): VerificationKey =
        walletFor_(peer).exportVerificationKey

    private def walletFor_(peer: TestPeerName): PeerWallet =
        TestPeers.walletCache.getOrElseUpdate(
          (seedPhrase.mnemonic, peer.ordinal),
          TestPeers.deriveScalusWallet(seedPhrase.mnemonic, peer.ordinal)
        )

    // Stays instance-scoped: unlike the key it is derived from, an address *is* network-specific.
    private val addressCache: mutable.Map[TestPeerName, ShelleyAddress] =
        mutable.Map.empty.withDefault(peer =>
            verificationKeyFor(peer).shelleyAddress()(using cardanoNetwork)
        )

}

object TestPeers:

    /** Peer wallets, shared across every [[TestPeers]] instance in the JVM.
      *
      * It has to outlive the instance: `TestPeers.arbitrary` builds a fresh one per ScalaCheck
      * sample, so an instance-scoped cache would re-derive every peer's key per sample. Keyed by
      * `(mnemonic, ordinal)` and **not** by network, because [[deriveScalusWallet]] does not
      * involve the network; only address encoding does, and that happens downstream of the key.
      * `TestPeersTest` pins that. The key space is therefore tiny: the few seed phrases in use
      * times at most `TestPeerName.maxPeers`.
      *
      * Safe to share — [[PeerWallet]] is an immutable holder of a key pair whose methods are pure —
      * and concurrent by construction, since suites run in parallel and share this map.
      */
    private val walletCache: TrieMap[(String, Int), PeerWallet] = TrieMap.empty

    /** Deterministic vanilla Ed25519 wallet for a peer, a pure function of `(mnemonic, ordinal)`.
      *
      * Replaces the former BloxBean / Ed25519-BIP32 derivation. Vanilla 32-byte keys round-trip
      * through the private-config JSON codec, so a config generated from these peers is actually
      * runnable — BIP32 extended keys serialize lossily (a dummy all-zero signing key) and boot a
      * node that cannot sign. No network input: the key is network-independent; only address
      * encoding downstream is network-specific.
      */
    def deriveScalusWallet(mnemonic: String, ordinal: Int): PeerWallet =
        val (vk, sk) = deriveScalusKeypair(mnemonic, ordinal)
        PeerWallet.scalusWallet(vk, sk)

    /** The raw keypair behind [[deriveScalusWallet]]. Exposed so a test that writes a peer's
      * `private.json` can splice in the real signing key explicitly — the stock config encoders
      * deliberately withhold it ([[PeerWallet.dummyPeerWalletEncoder]]).
      */
    def deriveScalusKeypair(mnemonic: String, ordinal: Int): (VerificationKey, SigningKey) = {
        val seed = MessageDigest
            .getInstance("SHA-256")
            .digest(s"$mnemonic#$ordinal".getBytes(StandardCharsets.UTF_8))
        val sk = Ed25519PrivateKeyParameters(seed, 0)
        val vk = VerificationKey.unsafeFromByteString(
          ByteString.fromArray(sk.generatePublicKey().getEncoded)
        )
        (vk, SigningKey.unsafeFromByteString(ByteString.fromArray(sk.getEncoded)))
    }

    def arbitrary: Gen[TestPeers] = for {
        spec <- TestPeersSpec.generate()
        testPeers <- generate(spec)
    } yield testPeers

    def apply(
        seedPhrase: SeedPhrase,
        network: CardanoNetwork,
        peersNumber: Int,
        coilPeersNumber: Int = 0,
    ): TestPeers =
        new TestPeers(seedPhrase, network, peersNumber, coilPeersNumber)

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

    /** [[TestPeers.walletCache]] is keyed by `(mnemonic, ordinal)` with no network component, which
      * is only sound because [[TestPeers.deriveScalusWallet]] is a pure function of the seed and
      * ordinal — no network, and no randomness. Pin that: the same seed and ordinal yield an
      * equivalently-signing wallet every time.
      */
    val _ = property("a peer's wallet is a deterministic function of (seed, ordinal)") =
        Prop.forAll(
          Gen.oneOf(SeedPhrase.Yaci, SeedPhrase.Public),
          Gen.choose(0, TestPeerName.maxPeers - 1)
        ) { (seedPhrase, ordinal) =>
            // Derive straight, bypassing the cache the property exists to justify.
            // PeerWallet's equality is extensional — same exported vkey *and* same signature over a
            // fixed message — so this covers the signing key, not just the public one.
            TestPeers.deriveScalusWallet(seedPhrase.mnemonic, ordinal) ==
                TestPeers.deriveScalusWallet(seedPhrase.mnemonic, ordinal)
        }
}
