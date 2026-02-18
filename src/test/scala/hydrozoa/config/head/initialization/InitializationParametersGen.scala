package hydrozoa.config.head.initialization

import cats.*
import cats.data.*
import cats.syntax.semigroup.*
import hydrozoa.config.head.initialization.InitializationParametersGenBottomUp.generateInitializationParameters
import hydrozoa.config.head.multisig.fallback.{FallbackContingency, FallbackContingencyGen, generateFallbackContingency}
import hydrozoa.config.head.network.CardanoNetwork.ensureMinAda
import hydrozoa.config.head.network.{CardanoNetwork, generateStandardCardanoNetwork}
import hydrozoa.config.head.peers.{TestPeers, generateTestPeers}
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.lib.cardano.scalus.given_Choose_Coin
import hydrozoa.lib.cardano.scalus.ledger.{asUtxoList, asUtxos}
import hydrozoa.lib.cardano.value.coin.Distribution.unsafeNormalizeWeights
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.JointLedger
import hydrozoa.multisig.ledger.dapp.token.CIP67.HeadTokenNames
import java.time.Instant
import monocle.syntax.all.*
import org.scalacheck.{Gen, Prop, Properties}
import scala.collection.immutable.SortedMap
import scala.concurrent.duration.DurationInt
import scalus.cardano.address.Address
import scalus.cardano.ledger.*
import scalus.cardano.ledger.TransactionOutput.{Babbage, valueLens}
import spire.math.{Rational, SafeLong}
import test.Generators.Hydrozoa.*
import test.Generators.Other.genCoinDistributionWithMinAdaUtxo
import test.TestPeer

// TODO: what do you think of expanding our shortening citizenship?
//   - generate -> gen
//   - initialization -> init or i12n ? (and the same for types)
//   - parameters -> params (and the same for types)

object HeadStartTimeGen {
    type HeadStartTimeGen = SlotConfig => Gen[QuantizedInstant]

    /** Generate [[headStartTime]] between 0 and 10 years from the zero slot time. Good for all
      * tests but Yaci-based ones.
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

    /** This "generator" checks that [[slotConfig]] is usable right now and returns the current time
      * as the head start time under given slot configuration. This is supposed to be used with
      * Yaci, when we cannot set the time on L1.
      */
    final def currentTimeHeadStartTime(slotConfig: SlotConfig): Gen[QuantizedInstant] =
        val now = Instant.now()
        require(slotConfig.zeroSlot <= now.toEpochMilli, "zero slot time cannot be in the future")
        Gen.const(
          QuantizedInstant(
            slotConfig = slotConfig,
            instant = now
          )
        )
}

object InitializationParametersGenTopDown {
    import HeadStartTimeGen.*
    import CappedValueGen.*

    type GenInitializationParameters =
        (testPeers: TestPeers) => (
            generateCardanoNetwork: Gen[CardanoNetwork],
            generateHeadStartTime: HeadStartTimeGen,
            generateFallbackContingency: FallbackContingencyGen,
            generateGenesisUtxosL1: GenesisUtxosGen,
            equityRange: (Coin, Coin)
        ) => Gen[
          InitializationParameters
        ]

    type GenInitializationParameters2 =
        (
            generateCardanoNetwork: Gen[CardanoNetwork],
            generateHeadStartTime: HeadStartTimeGen,
            generateFallbackContingency: FallbackContingencyGen,
            generateGenesisUtxosL1: GenesisUtxosGen,
            equityRange: (Coin, Coin)
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

    case class GenWithDeps(
        generator: GenInitializationParameters = generateInitializationParameters,
        generateGenesisUtxosL1: GenesisUtxosGen,
        equityRange: (Coin, Coin) = Coin(5_000_000) -> Coin(500_000_000)
    )

    /** This is a "bad" generator, that executes top-down approach that limits significantly its
      * coverage. It exists for a reason:
      *   - On Yaci you are limited to a set of initial utxos
      *   - Even if you can spawn utxos on Yaci you can't on a public testnet
      *   - When doing integration testing we don't want need to cover a wide space since we are
      *     very constrained in number of test cases we can run.
      *
      * Top-down here means that we start with some existing utxos for every peer and breaks them
      * into parts (contingency, change, initial deposits and so on).
      *
      * Support multi assets.
      */
    def generateInitializationParameters(testPeers: TestPeers)(
        generateCardanoNetwork: Gen[CardanoNetwork] = generateStandardCardanoNetwork,
        generateHeadStartTime: HeadStartTimeGen = currentTimeHeadStartTime,
        generateFallbackContingency: FallbackContingencyGen = generateFallbackContingency,
        generateGenesisUtxosL1: GenesisUtxosGen = generateRandomPeersUtxosL1,
        equityRange: (Coin, Coin) = Coin(5_000_000) -> Coin(500_000_000)
    ): Gen[InitializationParameters] =
        for {
            cardanoNetwork <- generateCardanoNetwork
            headStartTime <- generateHeadStartTime(cardanoNetwork.slotConfig)
            fallbackContingency <- generateFallbackContingency(cardanoNetwork)
            genesisUtxos <- generateGenesisUtxosL1(cardanoNetwork)

            // We are calculating equity upfront to avoid using .suchThat
            totalEquity <- Gen.choose(equityRange._1, equityRange._2)
            peersEquity: SortedMap[HeadPeerNumber, Coin] <-
                Gen.listOfN(
                  testPeers.nHeadPeers - 1,
                  Gen.frequency(3 -> Gen.const(0), 7 -> Gen.choose(1, 10))
                ).map(tail => NonEmptyList.apply(1, tail))
                    .map(ws => unsafeNormalizeWeights[Int](ws, Rational.apply))
                    .map(_.distribute(SafeLong.apply(totalEquity.value)))
                    .map(_.zipWithIndex)
                    .map(
                      _.map((coinSL, ix) => HeadPeerNumber(ix) -> Coin(coinSL.getLong.get)).toList
                          .to(SortedMap)
                    )

            // Peers' contributions
            contributions <- Gen.sequence[List[Contribution], Contribution](
              testPeers.headPeerNums
                  .map(hpn =>
                      generatePeerContribution(
                        headPeerNumber = hpn,
                        peerUtxos = genesisUtxos(hpn),
                        fallbackContingency = fallbackContingency,
                        peerEquity = peersEquity(hpn),
                        cardanoNetwork = cardanoNetwork
                      )
                  )
                  .toList
            )

            // Combining together, .get is safe since it's derived from non-empty [[testPeers]]
            total = Semigroup.combineAllOption(contributions).get
            seedUtxo <- Gen.oneOf(total.fundingUtxos)
            genesisId = JointLedger.mkGenesisId(HeadTokenNames(seedUtxo.input).treasuryTokenName, 0)
            initialL2Utxos =
                total.l2transactionOutput.zipWithIndex
                    .map((o, ix) => TransactionInput(transactionId = genesisId, index = ix) -> o)
                    .toMap

        } yield InitializationParameters(
          headStartTime = headStartTime,
          initialL2Utxos = initialL2Utxos,
          initialEquityContributions = NonEmptyMap.fromMapUnsafe(peersEquity),
          initialSeedUtxo = seedUtxo,
          initialAdditionalFundingUtxos = total.fundingUtxos.asUtxos - seedUtxo.input,
          initialChangeOutputs = total.changeOutputs
        )

    // ===================================
    // Semigroup contributions for initialization params
    // ===================================

    case class Contribution(
        fundingUtxos: List[Utxo],
        changeOutputs: List[TransactionOutput],
        l2transactionOutput: List[TransactionOutput],
    )

    implicit val contributionSemigroup: Semigroup[Contribution] =
        Semigroup.instance(
          cmb = (a, b) =>
              Contribution(
                fundingUtxos = a.fundingUtxos |+| b.fundingUtxos,
                changeOutputs = a.changeOutputs |+| b.changeOutputs,
                l2transactionOutput = a.l2transactionOutput |+| b.l2transactionOutput
              )
        )

    def generatePeerContribution(
        headPeerNumber: HeadPeerNumber,
        peerUtxos: Utxos,
        fallbackContingency: FallbackContingency,
        peerEquity: Coin,
        cardanoNetwork: CardanoNetwork
    ): Gen[Contribution] = {
        val peerAddresses = peerUtxos.values.map(_.address).toSet

        for {
            // 1. Funding utxos, at least one since individual contingency is mandatory
            fundingUtxos <- Gen.atLeastOne(peerUtxos.asUtxoList)
            fundingValue = fundingUtxos.map(_.output.value).fold(Value.zero)(_ + _)

            // 2. Subtracting contingency and equity
            contingency = fallbackContingency.totalContingencyFor(headPeerNumber)
            netFundingValue = fundingValue - Value.lovelace(contingency.value)
            maxChange = netFundingValue - Value.lovelace(peerEquity.value)

            // 3. Generate change
            change <- generateCappedValue(cardanoNetwork)(capValue = maxChange)
            changeAddress <- Gen.oneOf(peerAddresses)
            rest = maxChange - change

            // Generate L2 utxos from the rest (if present)
            l2TransactionOutputs <- Gen.tailRecM(List.empty[Babbage] -> rest)((acc, rest) =>
                for {
                    next <- generateCappedValue(cardanoNetwork)(capValue = rest)
                    address <- Gen.oneOf(peerAddresses)
                    acc_ = acc :+ Babbage(address, next)
                } yield
                    if next == rest
                    then Right(acc_)
                    else Left(acc_ -> (rest - next))
            )

        } yield Contribution(
          fundingUtxos = fundingUtxos.toList,
          changeOutputs = List(TransactionOutput(address = changeAddress, value = change)),
          l2transactionOutput = l2TransactionOutputs
        )
    }
}

object SanityCheck extends Properties("Initialization Parameters Sanity Check") {
    val _ = property("sanity check") = Prop.forAll(generateTestPeers())(testPeers =>
        Prop.forAll(
          InitializationParametersGenTopDown.generateInitializationParameters(testPeers)()
        )(_ => true)
    )
}

object CappedValueGen:

    /** Generate a small Value out of a big Value. The main workhorse to split up values.
      *
      * Invariants:
      *   - minAda requirement here leniently means "a value satisfies it being in a Babbage output
      *     locked at the base address with no datum/script"
      *   - The generated Value always satisfies it
      *   - The rest Value either does it alike, or it is empty
      *
      * To generate a small Coin (lovelace) out of a big Coin, chooses an amount between minLovelace
      * and the total amount in the big Coin.
      *
      * To generate a small MultiAsset out of a big MultiAsset:
      *   - Select a non-empty subset of the policy IDs.
      *   - For each selected policy ID, select a non-empty subset of the token names.
      *   - For each selected (policyID, tokenName), choose an amount between 1 and the total amount
      *     in the big MultiAsset.
      *
      * @param cardanoNetwork
      *   Used to enforce minAda requirement
      * @param capValue
      *   Value available
      * @param maxLovelace
      *   Prevents choosing more lovelaces that specified
      * @param maxToken
      *   Prevents choosing more tokens (of every kind) that specified
      * @return
      *   Value in gen or error if minAda condition cannot be satisfied
      */
    def generateCappedValue(
        cardanoNetwork: CardanoNetwork
    )(
        capValue: Value,
        maxLovelace: Option[Long] = None,
        maxToken: Option[Long] = None
    ): Gen[Value] =

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
          ensureMinAdaLenient(capValue) == capValue,
          s"maxValue itself should satisfy minAda condition: minimal: ${ensureMinAdaLenient(capValue)}, actual value: ${capValue}"
        )

        for {
            lovelace <- Gen.choose(0L, maxLovelace.getOrElse(capValue.coin.value)).map(Coin.apply)
            policySubset <- Gen.someOf(capValue.assets.assets.toSeq)
            assetSubset <- Gen.sequence[Seq[
              (PolicyId, SortedMap[AssetName, Long])
            ], (PolicyId, SortedMap[AssetName, Long])](
              policySubset.map { case (policyId, tokenMap) =>
                  Gen.someOf(tokenMap.toSeq)
                      .suchThat(_.nonEmpty)
                      .flatMap(tokens =>
                          Gen.sequence[Seq[(AssetName, Long)], (AssetName, Long)](
                            tokens.map { case (assetName, maxAmount) =>
                                Gen.choose(1L, maxToken.getOrElse(maxAmount)).map(assetName -> _)
                            }
                          )
                      )
                      .map(tokens => policyId -> SortedMap.from(tokens))
              }
            )
            assets = MultiAsset.fromAssets(SortedMap.from(assetSubset))
            value = ensureMinAdaLenient(Value(lovelace, assets))
            rest = capValue - value
        } yield
            if ensureMinAdaLenient(rest) == rest
            then value
            else capValue
end CappedValueGen

object InitializationParametersGenBottomUp {
    import HeadStartTimeGen.*

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
}

object SanityCheckBottomUp extends Properties("Initialization Parameters Sanity Check") {
    val _ = property("sanity check") = Prop.forAll(generateTestPeers())(testPeers =>
        Prop.forAll(generateInitializationParameters(testPeers)())(_ => true)
    )
}
