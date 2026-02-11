package hydrozoa.config.head.initialization

import cats.*
import cats.data.*
import hydrozoa.config.head.multisig.fallback.{FallbackContingencyGen, generateFallbackContingency}
import hydrozoa.config.head.network.{CardanoNetwork, generateStandardCardanoNetwork}
import hydrozoa.config.head.peers.{TestPeers, generateTestPeers}
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import java.time.Instant
import monocle.syntax.all.*
import org.scalacheck.{Gen, Prop, Properties}
import scala.collection.immutable.SortedMap
import scala.concurrent.duration.DurationInt
import scalus.cardano.ledger.*
import scalus.cardano.ledger.TransactionOutput.valueLens
import test.Generators.Hydrozoa.*
import test.Generators.Other.genCoinDistributionWithMinAdaUtxo
import test.TestPeer

type HeadStartTimeGen = SlotConfig => Gen[QuantizedInstant]

/** Generate [[headStartTime]] between 0 and 10 years from the zero slot time. Good for all tests
  * but Yaci-based ones.
  */
def generateHeadStartTime(slotConfig: SlotConfig): Gen[QuantizedInstant] =
    Gen
        .choose(0L, 10 * 365.days.toSeconds)
        .map(offsetSeconds =>
            QuantizedInstant(
              slotConfig = slotConfig,
              instant = Instant.ofEpochMilli(slotConfig.zeroTime + offsetSeconds * 1_000)
            )
        )

/** This "generator" checks that [[slotConfig]] is usable right now and returns the current time as
  * the head start time under given slot configuration. This is supposed to be used with Yaci, when
  * we cannot set the time on L1.
  */
def currentTimeHeadStartTime(slotConfig: SlotConfig): Gen[QuantizedInstant] =
    val now = Instant.now()
    require(slotConfig.zeroSlot <= now.toEpochMilli, "zero slot time cannot be in the future")
    Gen.const(
      QuantizedInstant(
        slotConfig = slotConfig,
        instant = now
      )
    )

/** @param generateHeadPeers
  * @param generateCardanoNetwork
  * @return
  *
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

type GenInitializationParameters =
    TestPeers => (Gen[CardanoNetwork], HeadStartTimeGen, FallbackContingencyGen) => Gen[
      InitializationParameters
    ]

type GenInitializationParameters2 =
    (Gen[CardanoNetwork], HeadStartTimeGen, FallbackContingencyGen) => Gen[InitializationParameters]

def generateInitializationParameters(testPeers: TestPeers)(
    generateCardanoNetwork: Gen[CardanoNetwork] = generateStandardCardanoNetwork,
    generateHeadStartTime: HeadStartTimeGen = currentTimeHeadStartTime,
    generateFallbackContingency: FallbackContingencyGen = generateFallbackContingency
): Gen[InitializationParameters] =
    for {
        cardanoNetwork <- generateCardanoNetwork
        headStartTime <- generateHeadStartTime(cardanoNetwork.slotConfig)

        fallbackContingency <- generateFallbackContingency(cardanoNetwork)

        // Helper generator for l2 utxos and seed utxo
        genUtxoFromKnownPeer: Gen[Utxo] =
            for {
                peer <- Gen.oneOf(testPeers._testPeers.toList).flatMap(_._2)
                utxo <- genAdaOnlyPubKeyUtxo(
                  config = cardanoNetwork,
                  peer = peer
                )
            } yield utxo

        l2Utxos <- {
            Gen
                .listOf(genUtxoFromKnownPeer)
                .map(list => Map.from(list.map(_.toTuple)))
        }
        l2Value = l2Utxos.values.map(_.value).fold(Value.zero)(_ + _)

        equityContributions <- generateEquityContributions(testPeers.headPeers.nHeadPeers)
        equity = equityContributions.toSortedMap.values.map(Value(_)).fold(Value.zero)(_ + _)

        // Need to limit the number so that we don't exceed tx size limits
        nChangeUtxos <- Gen.choose(1, 50)
        changeUtxos <- Gen.listOfN(nChangeUtxos, genUtxoFromKnownPeer)
        changeAmount = changeUtxos.map(_.output.value).fold(Value.zero)(_ + _)

        grossFundingAmount = equity
            + l2Value
            + Value(fallbackContingency.collectiveContingency.total)
            + Value.lovelace(
              fallbackContingency.individualContingency.total.value * testPeers.headPeers.nHeadPeers
            )
            + changeAmount

        fundingUtxosList <-
            for {
                nFundingUtxos <- Gen.choose(1, 100)
                utxos <- Gen.listOfN(nFundingUtxos, genUtxoFromKnownPeer)
                utxosWithZeroValue = utxos.map(
                  _.focus(_.output).andThen(valueLens).replace(Value.zero)
                )
                distributed <- genCoinDistributionWithMinAdaUtxo(
                  coin = grossFundingAmount.coin,
                  utxoList = NonEmptyList.fromListUnsafe(utxosWithZeroValue),
                  params = cardanoNetwork.cardanoProtocolParams
                )
            } yield distributed

        seedUtxo = fundingUtxosList.head
        additionalFundingUtxos: Utxos = Map.from(
          fundingUtxosList.toList.map(_.toTuple)
        ) - seedUtxo.input

    } yield InitializationParameters(
      headStartTime = headStartTime,
      initialL2Utxos = l2Utxos,
      initialEquityContributions = equityContributions,
      initialSeedUtxo = seedUtxo,
      initialAdditionalFundingUtxos = additionalFundingUtxos,
      initialChangeOutputs = changeUtxos.map(_.output)
    )

// TODO: improve?
def generateEquityContributions(numPeers: Int): Gen[NonEmptyMap[HeadPeerNumber, Coin]] =
    for {
        shares <- Gen.listOfN(numPeers, Gen.choose(5_000_000, 500_000_000).map(Coin(_)))
        peerShares = NonEmptyMap.fromMapUnsafe(SortedMap.from(shares.zipWithIndex.map {
            case (share, index) =>
                HeadPeerNumber(index) -> share
        }.toMap))

    } yield peerShares

def arbitraryHeadPeerUtxo(cardanoNetwork: CardanoNetwork, peer: TestPeer): Gen[Utxo] =
    genAdaOnlyPubKeyUtxo(
      config = cardanoNetwork,
      peer = peer
    )

def pickHeadPeerUtxo(availableUtxos: Utxos, peer: TestPeer) = ???

object SanityCheck extends Properties("Initialization Parameters Sanity Check") {
    val _ = property("sanity check") = Prop.forAll(generateTestPeers())(testPeers =>
        Prop.forAll(generateInitializationParameters(testPeers)())(_ => true)
    )
}
