package hydrozoa.config.head.initialization

import cats.*
import cats.data.*
import hydrozoa.config.head.multisig.fallback.FallbackContingency.mkFallbackContingencyWithDefaults
import hydrozoa.config.head.network.{CardanoNetwork, cardanoNetworkGenerator}
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import monocle.Focus
import org.scalacheck.{Gen, Prop, Properties}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.TransactionOutput.valueLens
import scalus.|>
import test.Generators.Hydrozoa.*
import test.Generators.Other.genCoinDistributionWithMinAdaUtxo
import test.{TestPeer, testPeersGenerator}

import java.time.Instant
import scala.collection.immutable.SortedMap
import scala.concurrent.duration.DurationInt

def genEquityShares(numPeers: Int)(
    genNetwork: Option[Gen[CardanoNetwork]]
): Gen[(NonEmptyMap[HeadPeerNumber, Coin])] =
  for {
    shares <- Gen.listOfN(numPeers, Gen.choose(5_000_000, 500_000_000).map(Coin(_)))
        peerShares = NonEmptyMap.fromMapUnsafe(SortedMap.from(shares.zipWithIndex.map {
            case (share, index) =>
                HeadPeerNumber(index) -> share
        }.toMap))

    } yield (
      peerShares
    )

/** @param generateHeadPeers
  * @param generateCardanoNetwork
  * @return
  *   - A [[headStartTime]] between 0 and 10 years from the zero slot time
  *   - An L2 utxo set that contains only pub key, ada-only utxos, spendable by some test peer
  *   - A seed utxo from a known peer with enough ada to cover the l2 utxo set
  *   - Arbitrary additional funding utxos from known peers, with a 10 to 10k ADA surplus
  *   - Exactly 4 change utxos, ada only, pubkey from a known peer, with the excess funding
  *     distributed.
  */
// TODO:
// - Choose variable number of change utxos in advance, estimate surplus funding to cover at least min ADA
// - Non ADA assets in funding utxos
// - Distribute equity more randomly among seed + funding utxos
def initializationParametersGen(testPeers: NonEmptyList[TestPeer])(
    generateCardanoNetwork: Option[Gen[CardanoNetwork]],
): Gen[InitializationParameters] =
    for {
        // ===================================
        // General data extraction and setup
        // ===================================
        cardanoNetwork <- generateCardanoNetwork.getOrElse(cardanoNetworkGenerator)
        // Hardcoded to 3,3 right now. We can do a better estimate later
        fallbackContingency = cardanoNetwork.mkFallbackContingencyWithDefaults(Coin.ada(3), Coin.ada(3))
        // Helper generator for l2 utxos and seed utxo
        genUtxoFromKnownPeer: Gen[Utxo] =
          for {
            peer <- Gen.oneOf(testPeers.toList)
            utxo <- genAdaOnlyPubKeyUtxo(
              config = cardanoNetwork,
              peer = peer
            )
          } yield utxo

        // ===================================
        // actual value generation
        // ===================================    
        headStartTime <-
            Gen
                .choose(0L, 10 * 365.days.toSeconds)
                .map(offsetSeconds =>
                    QuantizedInstant(
                      slotConfig = cardanoNetwork.slotConfig,
                      instant = Instant.ofEpochMilli(cardanoNetwork.slotConfig.zeroTime + offsetSeconds * 1000)
                    )
                )
        

        initialL2Utxos <- {
            Gen
                .listOf(genUtxoFromKnownPeer)
                .map(list => Map.from(list.map(_.toTuple)))
        }
        initialL2Value = initialL2Utxos.values.map(_.value).fold(Value.zero)(_ + _)

   
        equityShares <- genEquityShares(testPeers.length)(
            Some(Gen.const(cardanoNetwork))
        )
        equity = equityShares.toSortedMap.values.map(Value(_)).fold(Value.zero)(_ + _)

        
        changeUtxos <- Gen.listOf(genUtxoFromKnownPeer)
        changeAmount = changeUtxos.map(_.output.value).fold(Value.zero)(_ + _)
                
        grossFundingAmount = equity
          + initialL2Value
          + Value(fallbackContingency.collectiveContingency.total)
          + Value.lovelace(fallbackContingency.individualContingency.total.value * testPeers.length)
          + changeAmount
          
        fundingUtxosList <-
            for {
                utxos <- Gen.nonEmptyListOf(genUtxoFromKnownPeer)
                distributed <- genCoinDistributionWithMinAdaUtxo(
                  coin = grossFundingAmount.coin,
                  utxoList = NonEmptyList.fromListUnsafe(utxos),
                  params = cardanoNetwork.cardanoProtocolParams
                )
            } yield distributed
        seedUtxo = fundingUtxosList.head
        additionalFundingUtxos: Utxos = Map.from(fundingUtxosList.toList.map(_.toTuple)) - seedUtxo.input
        
        initParams = InitializationParameters(
          headStartTime = headStartTime,
          initialL2Utxos = initialL2Utxos,
          initialEquityContributions = equityShares,
          initialSeedUtxo = seedUtxo,
          initialAdditionalFundingUtxos = additionalFundingUtxos,
          initialChangeOutputs = changeUtxos.map(_.output)
        )
    } yield initParams

object SanityCheck extends Properties("Initialization Parameters Sanity Check") {
    val _ = property("sanity check") = Prop.forAll(testPeersGenerator())(testPeers =>
        Prop.forAll(initializationParametersGen(testPeers)(generateCardanoNetwork = None))(_ =>
            true
        )
    )
}
