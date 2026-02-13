package hydrozoa.config.head.initialization

import cats.*
import cats.data.*
import cats.syntax.semigroup.*
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
import org.scalacheck.{Gen, Prop, Properties}
import scala.collection.immutable.SortedMap
import scala.concurrent.duration.DurationInt
import scalus.cardano.address.Address
import scalus.cardano.ledger.*
import spire.math.{Rational, SafeLong}

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
final def currentTimeHeadStartTime(slotConfig: SlotConfig): Gen[QuantizedInstant] =
    val now = Instant.now()
    require(slotConfig.zeroSlot <= now.toEpochMilli, "zero slot time cannot be in the future")
    Gen.const(
      QuantizedInstant(
        slotConfig = slotConfig,
        instant = now
      )
    )

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

/** This is a "bad" generator, that executes top-down approach that limits significantly its
  * coverage. It exists for a reason:
  *   - On Yaci you are limited to a set of initial utxos
  *   - Even if you can spawn utxos on Yaci you can't on a public testnet
  *   - When doing integration testing we don't want need to cover a wide space since we are very
  *     constrained in number of test cases we can run.
  *
  * Top-down here means that we start with some existing utxos for every peer and breaks them into
  * parts (contingency, change, initial deposits and so on).
  *
  * Support multi assets.
  */
def generateInitializationParametersTopDown(testPeers: TestPeers)(
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

object SanityCheck extends Properties("Initialization Parameters Sanity Check") {
    val _ = property("sanity check") = Prop.forAll(generateTestPeers())(testPeers =>
        Prop.forAll(generateInitializationParametersTopDown(testPeers)())(_ => true)
    )
}

// TODO: what do you think of expanding our shortening citizenship?
//   - generate -> gen
//   - initialization -> init or i12n ? (and the same for types)
//   - parameters -> params (and the same for types)

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
    val generateCappedValueC = generateCappedValue(cardanoNetwork)
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
      changeOutputs = List(TransactionOutput(address = changeAddress, value = change)),
      l2transactionOutput = l2TransactionOutputs
    )
}

/** Generate a small Value out of a big Value. The main workhorse to split up values.
  *
  * Invariants:
  *   - minAda requirement here leniently means "a value satisfies it being in a Babbage output
  *     locked at the base address with no datum/script"
  *   - The generated Value always satisfies it
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
