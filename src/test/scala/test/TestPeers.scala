package test

import cats.data.NonEmptyList
import com.bloxbean.cardano.client.account.Account
import com.bloxbean.cardano.client.common.model.Network as BloxbeanNetwork
import com.bloxbean.cardano.client.crypto.cip1852.DerivationPath.createExternalAddressDerivationPathForAccount
import hydrozoa.*
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.network.CardanoNetworkGen.given_Arbitrary_CardanoNetwork
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.lib.cardano.scalus.ShelleyAddressExtra.mkShelleyAddress
import hydrozoa.lib.cardano.scalus.txbuilder.Transaction.attachVKeyWitnesses
import hydrozoa.lib.cardano.wallet.WalletModule
import hydrozoa.lib.number.PositiveInt
import hydrozoa.multisig.consensus.peer.{HeadPeerId, HeadPeerNumber, HeadPeerWallet}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Test.Parameters
import org.scalacheck.{Gen, Prop, Properties}
import scala.collection.mutable
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.ArbitraryInstances.*
import scalus.cardano.ledger.{Transaction, VKeyWitness}
import scalus.crypto.ed25519.VerificationKey
import test.Generators.loggerGenerators

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
  *
  * @param seedPhrase
  * @param network
  * @param peersNumber
  */
case class TestPeers private (
    seedPhrase: SeedPhrase,
    network: CardanoNetwork,
    peersNumber: Int
) {
    import TestPeerName.maxPeers

    require(
      peersNumber <= maxPeers,
      s"The number of peers are limited to $maxPeers "
    )

    // ===================================
    // API
    // ===================================

    def mkHeadPeers: HeadPeers = HeadPeers(headPeerVKeys)

    def nHeadPeers: PositiveInt = PositiveInt.unsafeApply(peersNumber)

    private val peerNumbers: List[Int] = List.range(0, peersNumber)

    def headPeerNums: NonEmptyList[HeadPeerNumber] =
        NonEmptyList.fromListUnsafe(
          peerNumbers.map(ix => HeadPeerNumber(ix))
        )

    def headPeerIds: NonEmptyList[HeadPeerId] =
        NonEmptyList.fromListUnsafe(
          peerNumbers.map(ix => HeadPeerId(ix, peersNumber))
        )

    def headPeerVKeys: NonEmptyList[VerificationKey] =
        NonEmptyList.fromListUnsafe(
          peerNumbers.map(ix => verificationKeyFor(TestPeerName.fromOrdinal(ix)))
        )

    def verificationKeyFor(peerNumber: HeadPeerNumber): VerificationKey =
        verificationKeyFor(TestPeerName.fromOrdinal(peerNumber))

    def verificationKeyFor(peer: TestPeerName): VerificationKey =
        require(
          peer.ordinal < peersNumber,
          s"Can't access peer $peer there is only $peersNumber is the head"
        )
        VerificationKey.unsafeFromArray(bloxbeanAccountFor(peer).publicKeyBytes())

    def addressFor(peerNumber: HeadPeerNumber): ShelleyAddress =
        addressFor(TestPeerName.fromOrdinal(peerNumber))

    def addressFor(peer: TestPeerName): ShelleyAddress = {
        require(
          peer.ordinal < peersNumber,
          s"Can't access peer $peer there is only $peersNumber is the head"
        )
        addressCache.useOrCreate(peer)
    }

    def walletFor(peerNumber: HeadPeerNumber): HeadPeerWallet =
        walletFor(TestPeerName.fromOrdinal(peerNumber))

    def walletFor(peer: TestPeerName): HeadPeerWallet =
        require(
          peer.ordinal < peersNumber,
          s"Can't access peer $peer there is only $peersNumber is the head"
        )
        walletCache.useOrCreate(peer)

    /** This is needed here to sign the initialization tx, when we still don't have
      * [[MultiNodeConfig]].
      */
    def multisignTx(tx: Transaction): Transaction =
        tx.attachVKeyWitnesses(mkVKeyWitnesses(tx).toList)

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
              network.asBloxbeanNetwork,
              seedPhrase.mnemonic,
              createExternalAddressDerivationPathForAccount(peer.ordinal)
            )
        )

    private def bloxbeanAccountFor(peer: TestPeerName): Account = accountCache.useOrCreate(peer)

    private val addressCache: mutable.Map[TestPeerName, ShelleyAddress] =
        mutable.Map.empty.withDefault(peer =>
            mkShelleyAddress(verificationKeyFor(peer), network.network)
        )

    private val walletCache: mutable.Map[TestPeerName, HeadPeerWallet] = mutable.Map.empty
        .withDefault(peer => {
            val hdKeyPair = bloxbeanAccountFor(peer).hdKeyPair()
            HeadPeerWallet(
              HeadPeerNumber(peer.ordinal),
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

    def apply(seedPhrase: SeedPhrase, network: CardanoNetwork, peersNumber: Int): TestPeers =
        new TestPeers(seedPhrase, network, peersNumber)

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
        import CardanoNetwork.*

        self match {
            case Preprod => BloxbeanNetwork(0, 1)
            case Preview => BloxbeanNetwork(0, 2)
            case Mainnet => BloxbeanNetwork(1, 764824073)
            case _       => throw RuntimeException("Unexpected Cardano network")
        }

enum PeersNumberSpec:
    case Random
    case Range(mbMin: Option[Int] = None, mbMax: Option[Int] = None)
    case Exact(peersNumber: Int)

object PeersNumberSpec:
    import PeersNumberSpec.*

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

    val distinct = mutable.Set.empty[TestPeers]
    var hasLogged = false

    val _ = property("generates") = Prop.forAll(
      TestPeersSpec
          .generate()
          .flatMap(TestPeers.generate)
    )(testPeers => {
        val _ = distinct.add(testPeers)
        true
    })

    val _ = property("z-print-results") = Prop.forAllNoShrink(Gen.const(())) { _ =>
        if !hasLogged then {
            loggerGenerators.trace(
              distinct.toList
                  .map(_.toString)
                  .sorted
                  .mkString("Unique values:", "\n\t-", "\n\n-----")
            )
            hasLogged = true
        }
        Prop.passed
    }
}
