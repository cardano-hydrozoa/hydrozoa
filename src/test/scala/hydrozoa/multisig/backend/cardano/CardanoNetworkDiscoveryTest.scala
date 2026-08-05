package hydrozoa.multisig.backend.cardano

import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.multisig.backend.cardano.CardanoNetworkDiscovery.ChainGeometry
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.Network
import scalus.cardano.blockfrost.GenesisInfo
import scalus.cardano.ledger.{CardanoInfo, Coin}

/** Coverage for the part of discovery that decides anything: turning a chain's geometry into a
  * [[CardanoNetwork.Custom]]. The fetch itself needs a live backend; this assembly does not, so it
  * is tested directly.
  *
  * The geometry arrives one of two ways — read from a backend's `/genesis`, or told to
  * `discover-network` for a backend that serves none (a Yaci devnet answers 404 there). Both are
  * covered here, because both must be assembled and validated identically.
  */
class CardanoNetworkDiscoveryTest extends AnyFunSuite:

    private val params = CardanoInfo.preview.protocolParams

    /** A `/genesis` response shaped like a freshly created devnet's. Blockfrost reports
      * `system_start` and `slot_length` in **seconds**.
      */
    private def genesis(networkMagic: Int, slotLengthSeconds: Long = 1L): GenesisInfo =
        GenesisInfo(
          activeSlotsCoefficient = 0.05,
          updateQuorum = 5,
          maxLovelaceSupply = Coin(45_000_000_000_000_000L),
          networkMagic = networkMagic,
          epochLength = 432_000L,
          systemStart = 1_700_000_000L,
          slotsPerKesPeriod = 129_600L,
          slotLength = slotLengthSeconds,
          maxKesEvolutions = 62L,
          securityParam = 2_160L
        )

    /** Geometry as the harness reads it off a devnet's admin API. */
    private def told(magic: Long, slotLengthSeconds: Double = 1.0): ChainGeometry =
        ChainGeometry(
          systemStartSeconds = 1_700_000_000L,
          slotLengthSeconds = slotLengthSeconds,
          epochLength = 600L,
          protocolMagic = magic
        )

    test("a devnet's genesis becomes a Custom network with seconds scaled to milliseconds") {
        val magic = 42 // not one of the standard magics
        val geometry = ChainGeometry.fromGenesis(genesis(magic))
        CardanoNetworkDiscovery.mkCustomNetwork(geometry, params) match {
            case Left(message) => fail(s"expected a Custom network, got: $message")
            case Right(custom) =>
                val slotConfig = custom.cardanoInfo.slotConfig
                assert(
                  custom.protocolMagic == magic.toLong &&
                      custom.cardanoInfo.network == Network.Testnet &&
                      custom.cardanoInfo.protocolParams == params &&
                      // Blockfrost reports seconds; SlotConfig is milliseconds.
                      slotConfig.zeroTime == 1_700_000_000L * 1000L &&
                      slotConfig.slotLength == 1000L &&
                      slotConfig.epochLength == 432_000L &&
                      // Shelley-at-genesis: the chain starts at slot 0.
                      slotConfig.zeroSlot == 0L
                )
        }
    }

    test("a told geometry produces the same chain a genesis-reported one would") {
        // The two routes must not diverge: whether the backend described its chain or the caller
        // did, the same numbers have to yield the same description.
        val reported = CardanoNetworkDiscovery.mkCustomNetwork(
          ChainGeometry.fromGenesis(genesis(networkMagic = 42)),
          params
        )
        // Spelled out by hand — the same four numbers `genesis(42)` carries — so this compares the
        // two routes rather than one route with itself.
        val byHand = ChainGeometry(
          systemStartSeconds = 1_700_000_000L,
          slotLengthSeconds = 1.0,
          epochLength = 432_000L,
          protocolMagic = 42L
        )
        assert(CardanoNetworkDiscovery.mkCustomNetwork(byHand, params) == reported)
    }

    test("a slot length other than the 1s default is scaled, not assumed") {
        val slotConfig = CardanoNetworkDiscovery
            .mkCustomNetwork(told(magic = 42, slotLengthSeconds = 2.0), params)
            .fold(m => fail(m), _.cardanoInfo.slotConfig)
        assert(slotConfig.slotLength == 2000L)
    }

    test("a sub-second slot length survives, rounded rather than truncated") {
        // A told geometry can be fractional where `/genesis` cannot: GenesisInfo.slotLength is
        // integral, so a 0.5s devnet is only describable through the explicit route.
        val half = CardanoNetworkDiscovery
            .mkCustomNetwork(told(magic = 42, slotLengthSeconds = 0.5), params)
            .fold(m => fail(m), _.cardanoInfo.slotConfig.slotLength)
        val whole = CardanoNetworkDiscovery
            .mkCustomNetwork(told(magic = 42, slotLengthSeconds = 1.0), params)
            .fold(m => fail(m), _.cardanoInfo.slotConfig.slotLength)
        assert(half == 500L && whole == 1000L, s"got $half and $whole")
    }

    test("a backend reporting a standard chain's magic is refused, by name") {
        List(CardanoNetwork.Mainnet, CardanoNetwork.Preprod, CardanoNetwork.Preview).foreach {
            standard =>
                val result = CardanoNetworkDiscovery.mkCustomNetwork(
                  ChainGeometry.fromGenesis(genesis(standard.protocolMagic.toInt)),
                  params
                )
                val name = standard.toString.toLowerCase
                assert(
                  result.left.exists(_.contains(name)),
                  s"a backend on $name must be configured as $name, not discovered; got $result"
                )
        }
    }

    test("a told standard magic is refused too, not just a reported one") {
        // The explicit route must not be a way around the check: it is the same assembly.
        val result = CardanoNetworkDiscovery.mkCustomNetwork(
          told(magic = CardanoNetwork.Preview.protocolMagic),
          params
        )
        assert(result.left.exists(_.contains("preview")), s"got $result")
    }

    test("a zero slot or epoch length is refused") {
        // Both are divisors in SlotConfig; left alone they would surface as an ArithmeticException
        // at node startup, long after this description was pinned into the head config.
        val zeroSlotLength = told(magic = 42, slotLengthSeconds = 0.0)
        val zeroEpochLength = told(magic = 42).copy(epochLength = 0L)
        assert(
          CardanoNetworkDiscovery
              .mkCustomNetwork(zeroSlotLength, params)
              .left
              .exists(_.contains("slotLength")) &&
              CardanoNetworkDiscovery
                  .mkCustomNetwork(zeroEpochLength, params)
                  .left
                  .exists(_.contains("epochLength"))
        )
    }
