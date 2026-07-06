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
import org.http4s.Uri
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Test.Parameters
import org.scalacheck.{Gen, Prop, Properties}
import scala.collection.immutable.SortedMap
import scala.collection.mutable
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.{Transaction, VKeyWitness}
import scalus.crypto.ed25519.VerificationKey
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
    coilPeersNumber: Int = 0,
) extends CardanoNetwork.Section,
      HeadPeers.Section {
    import TestPeerName.maxPeers

    private val peerNumbers: List[Int] = List.range(0, peersNumber)

    private def _require(peer: TestPeerName): Unit =
        require(
          peer.ordinal < peersNumber,
          s"Can't access peer $peer there is only $peersNumber is the head"
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

    override def headPeers: HeadPeers = {
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
        VerificationKey.unsafeFromArray(bloxbeanAccountFor(peer).publicKeyBytes())

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
        walletCache.useOrCreate(peer)

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
        walletCache.useOrCreate(TestPeerName.fromOrdinal(peersNumber + n.convert))

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

    private val addressCache: mutable.Map[TestPeerName, ShelleyAddress] =
        mutable.Map.empty.withDefault(peer =>
            verificationKeyFor(peer).shelleyAddress()(using cardanoNetwork)
        )

    private val walletCache: mutable.Map[TestPeerName, PeerWallet] = mutable.Map.empty
        .withDefault(peer => {
            val hdKeyPair = bloxbeanAccountFor(peer).hdKeyPair()
            PeerWallet(
              WalletModule.BloxBean,
              hdKeyPair.getPublicKey,
              hdKeyPair.getPrivateKey
            )
        })

}

object TestPeers:

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
    def withPeersNumberSpec(spec: PeersNumberSpec) = this.copy(peersNumberSpec = spec)
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
}
