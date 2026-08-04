package hydrozoa.multisig.backend.cardano

import hydrozoa.config.head.network.CardanoNetwork
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.Network
import scalus.cardano.blockfrost.GenesisInfo
import scalus.cardano.ledger.{CardanoInfo, Coin}

/** Coverage for the part of discovery that decides anything: turning what a Blockfrost-compatible
  * backend reported about its chain into a [[CardanoNetwork.Custom]]. The fetch itself needs a live
  * backend; this assembly does not, so it is tested directly.
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

    test("a devnet's genesis becomes a Custom network with seconds scaled to milliseconds") {
        val magic = 42 // not one of the standard magics
        CardanoNetworkDiscovery.mkCustomNetwork(genesis(magic), params) match {
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

    test("a sub-second slot length survives the conversion") {
        // Guards the scaling order: seconds must be scaled before any truncation.
        val slotConfig = CardanoNetworkDiscovery
            .mkCustomNetwork(genesis(networkMagic = 42, slotLengthSeconds = 2L), params)
            .fold(m => fail(m), _.cardanoInfo.slotConfig)
        assert(slotConfig.slotLength == 2000L)
    }

    test("a backend reporting a standard chain's magic is refused, by name") {
        List(CardanoNetwork.Mainnet, CardanoNetwork.Preprod, CardanoNetwork.Preview).foreach {
            standard =>
                val result = CardanoNetworkDiscovery.mkCustomNetwork(
                  genesis(standard.protocolMagic.toInt),
                  params
                )
                val name = standard.toString.toLowerCase
                assert(
                  result.left.exists(_.contains(name)),
                  s"a backend on $name must be configured as $name, not discovered; got $result"
                )
        }
    }
