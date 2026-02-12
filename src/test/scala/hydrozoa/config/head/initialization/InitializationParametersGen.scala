package hydrozoa.config.head.initialization

import cats.*
import cats.data.*
import cats.syntax.semigroup.*
import hydrozoa.config.head.multisig.fallback.{FallbackContingency, FallbackContingencyGen, generateFallbackContingency}
import hydrozoa.config.head.network.CardanoNetwork.ensureMinAda
import hydrozoa.config.head.network.{CardanoNetwork, generateStandardCardanoNetwork}
import hydrozoa.config.head.peers.{TestPeers, generateTestPeers}
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.lib.cardano.scalus.ledger.{asUtxoList, asUtxos}
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.JointLedger
import hydrozoa.multisig.ledger.dapp.token.CIP67.HeadTokenNames
import java.time.Instant
import org.scalacheck.{Gen, Prop, Properties}
import scala.collection.immutable.SortedMap
import scala.concurrent.duration.DurationInt
import scalus.cardano.address.Address
import scalus.cardano.ledger.*
import test.Generators.Hydrozoa.*
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

type GenInitializationParameters =
    TestPeers => (
        Gen[CardanoNetwork],
        HeadStartTimeGen,
        FallbackContingencyGen,
        GenesisUtxosGen
    ) => Gen[
      InitializationParameters
    ]

type GenInitializationParameters2 =
    (
        Gen[CardanoNetwork],
        HeadStartTimeGen,
        FallbackContingencyGen,
        GenesisUtxosGen
    ) => Gen[
      InitializationParameters
    ]

type GenesisUtxosGen = CardanoNetwork => Gen[Map[HeadPeerNumber, Utxos]]

/** Returns test genesis utxo provided by [[TestPeers]].
  */
def testPeersGenesisUtxosL1(testPeers: TestPeers)(
    network: CardanoNetwork
): Gen[Map[HeadPeerNumber, Utxos]] =
    Gen.const(testPeers.genesisUtxos(network.cardanoInfo.network))

/** Generate fake utxos, that don't exist, but are spendable using peers' credentials.
  */
def generateRandomPeersUtxosL1(network: CardanoNetwork): Gen[Map[HeadPeerNumber, Utxos]] = ???

/**   - An L2 utxo set that contains only pub key, ada-only utxos, spendable by some test peer
  *   - A seed utxo from a known peer with enough ada to cover the l2 utxo set
  *   - Arbitrary additional funding utxos from known peers, with a 10 to 10k ADA surplus
  *   - Exactly 4 change utxos, ada only, pubkey from a known peer, with the excess funding
  *     distributed.
  *
  * TODO:
  *   - Choose variable number of change utxos in advance, estimate surplus funding to cover at
  *     least min ADA
  *   - Non ADA assets in funding utxos
  *   - Distribute equity more randomly among seed + funding utxos
  */
def generateInitializationParameters(testPeers: TestPeers)(
    generateCardanoNetwork: Gen[CardanoNetwork] = generateStandardCardanoNetwork,
    generateHeadStartTime: HeadStartTimeGen = currentTimeHeadStartTime,
    generateFallbackContingency: FallbackContingencyGen = generateFallbackContingency,
    generateGenesisUtxosL1: GenesisUtxosGen = generateRandomPeersUtxosL1
): Gen[InitializationParameters] =
    for {
        cardanoNetwork <- generateCardanoNetwork
        headStartTime <- generateHeadStartTime(cardanoNetwork.slotConfig)

        fallbackContingency <- generateFallbackContingency(cardanoNetwork)

        //// Helper generator for l2 utxos
        // genUtxoFromKnownPeer: Gen[Utxo] =
        //    for {
        //        peer <- Gen.oneOf(testPeers._testPeers.toList).flatMap(_._2)
        //        utxo <- genAdaOnlyPubKeyUtxo(
        //          config = cardanoNetwork,
        //          peer = peer
        //        )
        //    } yield utxo
        //
        // l2Utxos <- {
        //    Gen
        //        .listOf(genUtxoFromKnownPeer)
        //        .map(list => Map.from(list.map(_.toTuple)))
        // }
        // l2Value = l2Utxos.values.map(_.value).fold(Value.zero)(_ + _)
        //
        // equityContributions <- generateEquityContributions(testPeers.headPeers.nHeadPeers)
        // equity = equityContributions.toSortedMap.values.map(Value(_)).fold(Value.zero)(_ + _)
        //
        //// Need to limit the number so that we don't exceed tx size limits
        // nChangeUtxos <- Gen.choose(1, 50)
        // changeUtxos <- Gen.listOfN(nChangeUtxos, genUtxoFromKnownPeer)
        // changeAmount = changeUtxos.map(_.output.value).fold(Value.zero)(_ + _)
        //
        // grossFundingAmount = equity
        //    + l2Value
        //    + Value(fallbackContingency.collectiveContingency.total)
        //    + Value.lovelace(
        //      fallbackContingency.individualContingency.total.value * testPeers.headPeers.nHeadPeers
        //    )
        //    + changeAmount
        //
        // fundingUtxosList <-
        //    for {
        //        nFundingUtxos <- Gen.choose(1, 100)
        //        utxos <- Gen.listOfN(nFundingUtxos, genUtxoFromKnownPeer)
        //        utxosWithZeroValue = utxos.map(
        //          _.focus(_.output).andThen(valueLens).replace(Value.zero)
        //        )
        //        distributed <- genCoinDistributionWithMinAdaUtxo(
        //          coin = grossFundingAmount.coin,
        //          utxoList = NonEmptyList.fromListUnsafe(utxosWithZeroValue),
        //          params = cardanoNetwork.cardanoProtocolParams
        //        )
        //    } yield distributed
        //
        // seedUtxo = fundingUtxosList.head
        // additionalFundingUtxos: Utxos = Map.from(
        //  fundingUtxosList.toList.map(_.toTuple)
        // ) - seedUtxo.input

        genesisUtxos <- generateGenesisUtxosL1(cardanoNetwork)

        // TODO: factor out mkGeneratePeerEquity?
        generatePeerEquity: (Coin => Gen[Coin]) = coin =>
            testPeers.nHeadPeers.convert match {
                // One peer - big enough contribution
                case 1 => Gen.choose(50_000_000L, coin.value).flatMap(Coin.apply)
                // Two peers - smaller contributions
                case 2 => Gen.choose(20_000_000L, coin.value).flatMap(Coin.apply)
                // Three and more peers - some zero, some maximal
                case _ =>
                    Gen.frequency(
                      2 -> Gen.const(Coin.zero),
                      7 -> Gen.choose(5_000_000L, coin.value).flatMap(Coin.apply),
                      1 -> Gen.const(coin.value).flatMap(Coin.apply)
                    )
            }
        contributions <- Gen.sequence[List[Contribution], Contribution](
          testPeers.headPeerNums
              .map(hpn =>
                  generatePeerContribution(
                    headPeerNumber = hpn,
                    peerUtxos = genesisUtxos(hpn),
                    fallbackContingency = fallbackContingency,
                    generatePeerEquity = generatePeerEquity,
                    cardanoNetwork = cardanoNetwork
                  )
              )
              .toList
        )

        total = Monoid.combineAll(contributions)
        seedUtxo <- Gen.oneOf(total.fundingUtxos)
        genesisId = JointLedger.mkGenesisId(HeadTokenNames(seedUtxo.input).treasuryTokenName, 0)
        initialL2Utxos =
            total.l2transactionOutput.zipWithIndex
                .map((o, ix) => TransactionInput(transactionId = genesisId, index = ix) -> o)
                .toMap

    } yield InitializationParameters(
      headStartTime = headStartTime,
      initialL2Utxos = initialL2Utxos,
      initialEquityContributions = NonEmptyMap.fromMapUnsafe(total.equityContribution),
      initialSeedUtxo = seedUtxo,
      initialAdditionalFundingUtxos = total.fundingUtxos.asUtxos - seedUtxo.input,
      initialChangeOutputs = total.changeOutputs
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

// TODO: what do you think of expanding out shortening citizenship?
//   - generate -> gen
//   - initialization -> init or i12n ?
//   - parameters -> params

// ===================================
// Monoidal contributions for initialization params
// ===================================

case class Contribution(
    fundingUtxos: List[Utxo] = List.empty,
    equityContribution: SortedMap[HeadPeerNumber, Coin] = SortedMap.empty,
    changeOutputs: List[TransactionOutput] = List.empty,
    l2transactionOutput: List[TransactionOutput] = List.empty
)

implicit val contributionMonoid: Monoid[Contribution] =
    Monoid.instance(
      emptyValue = Contribution(),
      cmb = (a, b) =>
          Contribution(
            fundingUtxos = a.fundingUtxos |+| b.fundingUtxos,
            equityContribution = a.equityContribution |+| b.equityContribution,
            changeOutputs = a.changeOutputs |+| b.changeOutputs,
            l2transactionOutput = a.l2transactionOutput |+| b.l2transactionOutput
          )
    )

def generatePeerContribution(
    headPeerNumber: HeadPeerNumber,
    peerUtxos: Utxos,
    fallbackContingency: FallbackContingency,
    generatePeerEquity: Coin => Gen[Coin],
    cardanoNetwork: CardanoNetwork
): Gen[Contribution] = {
    val generateCappedValueC = generateCappedValue(cardanoNetwork)
    // This is a bit hacky, but should work
    val peerAddresses = peerUtxos.values.map(_.address).toSet
    for {
        // 1. Funding utxos
        fundingUtxos <- Gen.someOf(peerUtxos.asUtxoList)
        fundingValue = fundingUtxos.map(_.output.value).fold(Value.zero)(_ + _)
        // 2. Subtracting contingency
        contingency = fallbackContingency.totalContingencyFor(headPeerNumber)
        netFundingValue = fundingValue - Value.lovelace(contingency.value)
        // 3. Equity contribution using specified strategy
        equity <- generatePeerEquity(netFundingValue.coin)
        // 4. Generate change
        maxChange = netFundingValue - Value.lovelace(equity.value)
        change <- generateCappedValueC(maxChange)
        changeAddress <- Gen.oneOf(peerAddresses)
        rest = maxChange - change
        // Generate L2 utxos from the rest (if present)
        l2TransactionOutputs <- Gen.tailRecM(List.empty[TransactionOutput] -> rest)((acc, rest) =>
            for {
                next <- generateCappedValueC(rest)
                address <- Gen.oneOf(peerAddresses)
                acc_ = acc :+ TransactionOutput(address, next)
            } yield
                if next == rest
                then Right(acc_)
                else Left(acc_ -> (rest - next))
        )

    } yield Contribution(
      fundingUtxos = fundingUtxos.toList,
      equityContribution = SortedMap(headPeerNumber -> equity),
      changeOutputs = List(TransactionOutput(address = changeAddress, value = change)),
      l2transactionOutput = l2TransactionOutputs
    )
}

/** Generate a small Value out of a big Value. The main workhorse to split up values.
  *
  * Invariants:
  *   - minAda requirement here leniently means "a value satisfies it being in a Babbage output
  *     locked at the base address with no datum/script"
  *   - The generated Value always satisfied it
  *   - The rest Value either does it alike, or it is empty
  *
  * To generate a small Coin (lovelace) out of a big Coin, chooses an amount between minLovelace and
  * the total amount in the big Coin.
  *
  * To generate a small MultiAsset out of a big MultiAsset:
  *   - Select a non-empty subset of the policy IDs.
  *   - For each selected policy ID, select a non-empty subset of the token names.
  *   - For each selected (policyID, tokenName), choose an amount between 1 and the total amount in
  *     the big MultiAsset.
  *
  * @param max
  *   maximum value available
  * @param cardanoNetwork
  *   used to verify minAda requirement
  * @return
  *   value in gen or error if minAda condition cannot be satisfied
  */
def generateCappedValue(cardanoNetwork: CardanoNetwork)(maxValue: Value): Gen[Value] =

    def ensureMinAdaLenient(value: Value): Value = {
        val anyBaseShelleyAddressIsGood = Address.fromBech32(
          "addr1q8mcs4umxqvl7hevfr4ssmmek553yf6lz0efc5w0qqca7wf2t3k3pkagdu2ynj629x5sx4wdflrw2g3vzn4967msd6fs45mp5a"
        )
        TransactionOutput
            .apply(
              anyBaseShelleyAddressIsGood,
              value
            )
            .ensureMinAda(cardanoNetwork)
            .value
    }

    // We cannot split up a value which is too small itself
    require(
      ensureMinAdaLenient(maxValue) == maxValue,
      "maxValue itself should satisfy minAda condition"
    )

    for {
        lovelace <- Gen.choose(0L, maxValue.coin.value).map(Coin.apply)
        policySubset <- Gen.someOf(maxValue.assets.assets.toSeq)
        assetSubset <- Gen.sequence[Seq[
          (PolicyId, SortedMap[AssetName, Long])
        ], (PolicyId, SortedMap[AssetName, Long])](
          policySubset.map { case (policyId, tokenMap) =>
              Gen.someOf(tokenMap.toSeq)
                  .suchThat(_.nonEmpty)
                  .flatMap(tokens =>
                      Gen.sequence[Seq[(AssetName, Long)], (AssetName, Long)](
                        tokens.map { case (assetName, maxAmount) =>
                            Gen.choose(1L, maxAmount).map(assetName -> _)
                        }
                      )
                  )
                  .map(tokens => policyId -> SortedMap.from(tokens))
          }
        )
        assets = MultiAsset.fromAssets(SortedMap.from(assetSubset))
        value = ensureMinAdaLenient(Value(lovelace, assets))
        rest = maxValue - value
    } yield
        if ensureMinAdaLenient(rest) == rest
        then value
        else maxValue
