package hydrozoa.config.head.initialization

import cats.*
import cats.data.*
import hydrozoa.config.head.network.{CardanoNetwork, cardanoNetworkGen}
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import monocle.Focus
import org.scalacheck.Gen
import scalus.cardano.ledger.TransactionOutput.valueLens
import scalus.cardano.ledger.{Coin, SlotConfig, Utxo, Value}
import scalus.|>
import test.Generators.Hydrozoa.*
import test.TestPeer

import java.time.Instant
import scala.collection.immutable.SortedMap
import scala.concurrent.duration.DurationInt

def genEquityShares(
    genPeers: Option[Gen[NonEmptyList[TestPeer]]],
    genNetwork: Option[Gen[CardanoNetwork]]
): Gen[(NonEmptyList[TestPeer], NonEmptyMap[HeadPeerNumber, Coin])] =
    for {
        peers <- genPeers.getOrElse(test.genTestPeers())
        shares <- Gen.listOfN(peers.length, Gen.choose(5_000_000, 500_000_000).map(Coin(_)))
        peerShares = NonEmptyMap.fromMapUnsafe(SortedMap.from(shares.zipWithIndex.map { case (share, index) =>
            HeadPeerNumber(index) -> share
        }.toMap))

    } yield (
      peers,
      peerShares
    )

/** @param genTestPeers
  * @param genSlotConfig
  * @param genCardanoNetwork
  * @return
  *   - A [[headStartTime]] between 0 and 10 years from the zero slot time
  *   - An L2 utxo set that contains only pub key, ada-only utxos, spendable by some test peer
  *   - A seed utxo from a known peer with enough ada to cover the l2 utxo set
  *   - Arbitrary additional funding utxos from known peers
  */
def initializationParametersGen(
    genTestPeers: Option[Gen[NonEmptyList[TestPeer]]],
    genSlotConfig: Option[Gen[SlotConfig]],
    genCardanoNetwork: Option[Gen[CardanoNetwork]]
): Gen[(NonEmptyList[TestPeer], InitializationParameters)] =
    for {
        testPeers <- genTestPeers.getOrElse(test.genTestPeers())
        slotConfig <- genSlotConfig.getOrElse(
          Gen.oneOf(SlotConfig.mainnet, SlotConfig.preview, SlotConfig.preprod)
        )
        cardanoNetwork <- genCardanoNetwork.getOrElse(cardanoNetworkGen)

        headStartTime <-
            Gen
                .choose(0L, 10 * 365.days.toSeconds)
                .map(offsetSeconds =>
                    QuantizedInstant(
                      slotConfig = slotConfig,
                      instant = Instant.ofEpochMilli(slotConfig.zeroTime + offsetSeconds * 1000)
                    )
                )

        // Helper generator for l2 utxos and seed utxo
        genUtxoFromKnownPeer =
            for {
                peer <- Gen.oneOf(testPeers.toList)
                utxo <- genAdaOnlyPubKeyUtxo(
                    config = cardanoNetwork,
                    peer = peer
                )
            } yield utxo

        initialL2Utxos <- {
            Gen
                .listOf(genUtxoFromKnownPeer)
                .map(list => Map.from(list.map(_.toTuple)))
        }

        (_, equityShares) <- genEquityShares(Some(Gen.const(testPeers)), Some(Gen.const(cardanoNetwork)))

        seedUtxo <- genUtxoFromKnownPeer.map(utxo => 
            utxo |> Focus[Utxo](_.output)
                .andThen(valueLens)
                .andThen(Focus[Value](_.coin))
                .modify(coin => coin + Coin(initialL2Utxos.toList.map(_._2.value.coin.value).sum)))
        fundingUtxos <- Gen.nonEmptyListOf(genUtxoFromKnownPeer).map(list => Map.from(list.map(_.toTuple)))

        initParams = InitializationParameters(
          headStartTime = headStartTime,
          initialL2Utxos = initialL2Utxos,
          initialEquityContributions = equityShares,
          initialSeedUtxo = seedUtxo,
          initialAdditionalFundingUtxos = fundingUtxos,
          initialChangeOutputs = ???
        )
    } yield (testPeers, initParams)
