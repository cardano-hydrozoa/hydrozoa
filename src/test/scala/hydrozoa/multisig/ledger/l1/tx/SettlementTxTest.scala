package hydrozoa.multisig.ledger.l1.tx

import cats.data.*
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.head.multisig.timing.TxTiming
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.{BlockCreationEndTime, FallbackTxStartTime}
import hydrozoa.config.head.multisig.timing.TxTiming.RequestTimes.RequestValidityEndTime
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.lib.cardano.scalus.QuantizedTime.{QuantizedFiniteDuration, QuantizedInstant, quantize}
import hydrozoa.multisig.ledger.block.BlockVersion
import hydrozoa.multisig.ledger.commitment.KzgCommitment.KzgCommitment
import hydrozoa.multisig.ledger.eutxol2.tx.GenesisObligation
import hydrozoa.multisig.ledger.l1.txseq.SettlementTxSeq
import hydrozoa.multisig.ledger.l1.utxo.{DepositUtxo, Equity, MultisigTreasuryUtxo}
import java.util.concurrent.TimeUnit
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, Gen}
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.*
import scalus.cardano.ledger.ArbitraryInstances.given
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.LedgerToPlutusTranslation.getValue
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.onchain.plutus.prelude.Ord.<=
import scalus.cardano.onchain.plutus.prelude.{Option as SOption, Ord}
import scalus.cardano.onchain.plutus.v1
import scalus.cardano.onchain.plutus.v1.Value.valueOrd
import scalus.cardano.txbuilder.TransactionBuilder.ensureMinAda
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.Data.toData
import test.*
import test.Generators.Hydrozoa.*
import test.Generators.Other

def genDepositDatum(network: CardanoNetwork.Section): Gen[DepositUtxo.Datum] = {
    for {
        address <- genPubkeyAddress(network).map(
          LedgerToPlutusTranslation.getAddress(_).credential
        )
        datum <- genByteStringData
        deadline <- Gen.posNum[BigInt]
        refundAddress <- genPubkeyAddress(network).map(
          LedgerToPlutusTranslation.getAddress(_)
        )
        genData = Gen.frequency(
          (99, genByteStringData.map(data => SOption.Some(data))),
          (1, SOption.None)
        )
        refundDatum <- genData

    } yield DepositUtxo.Datum(
      DepositUtxo.Refund.Instructions.Onchain(
        address = refundAddress,
        datum = refundDatum,
        refundStart = QuantizedInstant(
          network.slotConfig,
          java.time.Instant.ofEpochMilli(deadline.toLong)
        ).instant.toEpochMilli
      )
    )
}

// FIXME: This way of generating deposit Utxos was fine for earlier iterations of hydrozoa.
// As we get closer to a real implementation, it needs to be revised to catch more corner cases.
// In particular, with the introduction of "Virtual Utxos" in the deposit utxo, and with the deposit amount
// set deterministically from the sum of the virtual utxos + fees necessary for refund, this
// generator DOES NOT produce actual semantically valid deposit utxos
//
/** @param zeroTime
  *   generates deposits at some random offset (in the future) from a "zero time"
  */
def genDepositUtxo(
    config: CardanoNetwork.Section & TxTiming.Section,
    headAddress: Option[ShelleyAddress] = None,
)(
    zeroTime: QuantizedInstant = QuantizedInstant(
      config.slotConfig,
      config.slotConfig.slotToInstant(config.slotConfig.zeroSlot)
    )
): Gen[DepositUtxo] =
    for {
        utxoId <- arbitrary[TransactionInput]
        headAddress_ = headAddress.getOrElse(genScriptAddress(config).sample.get)
        dd <- genDepositDatum(config)

        // Mock utxo to calculate minAda
        mockUtxo = Babbage(
          address = headAddress_,
          value = Value.zero,
          datumOption = Some(Inline(dd.toData)),
          scriptRef = None
        )

        depositMinAda = ensureMinAda(mockUtxo, config.cardanoProtocolParams).value.coin

        depositAmount <- arbitrary[Coin].map(_ + depositMinAda).map(Value(_))

        // NOTE: these genesis obligations are completely arbitrary and WILL NOT be coherent with the
        // deposit amount
        address <- arbitrary[ShelleyAddress]
        l2Outputs <- Gen
            .nonEmptyListOf(genGenesisObligation(config, address))
            .map(NonEmptyList.fromListUnsafe)

        // Generate some offset to the "zero slot" time.
        offsetFromZero <- Gen.posNum[Long]

        requestValidityEndTime =
            RequestValidityEndTime(zeroTime + FiniteDuration(offsetFromZero, TimeUnit.SECONDS))

    } yield DepositUtxo(
      utxoId = utxoId,
      address = headAddress_,
      datum = dd,
      value = depositAmount,
      l2Payload = GenesisObligation.serialize(l2Outputs),
      depositFee = Coin.zero,
      requestValidityEndTime = requestValidityEndTime,
      absorptionStartTime = config.txTiming.depositAbsorptionStartTime(requestValidityEndTime),
      absorptionEndTime = config.txTiming.depositAbsorptionEndTime(requestValidityEndTime)
    )

/** Generate a "standalone" settlement tx. */
def genSettlementTxSeqBuilder(config: HeadConfig)(
    estimatedFee: Coin = Coin(5_000_000L),
    // If passed, the kzg commitment will be set to the value.
    // If not, its randomly generated
    kzgCommitment: Option[KzgCommitment] = None,
    previousMajorBlockCreationEndTime: BlockCreationEndTime = BlockCreationEndTime(
      java.time.Instant.now().quantize(config.slotConfig)
    ),
    currentMajorBlockOffsetDuration: QuantizedFiniteDuration =
        10.seconds.quantize(config.slotConfig),
    txTiming: TxTiming = TxTiming.default(config.slotConfig)
): Gen[SettlementTxSeq.Build] = {
    val fallbackTxStartTime =
        config.txTiming.newFallbackStartTime(previousMajorBlockCreationEndTime)

    val blockCreationEndTime = BlockCreationEndTime(
      previousMajorBlockCreationEndTime + currentMajorBlockOffsetDuration
    )

    // A helper to generator empty, small, medium, large (up to 1000)
    def genHelper[T](gen: Gen[T]): Gen[Vector[T]] = Gen.sized(size =>
        Gen.frequency(
          (1, Gen.const(Vector.empty)),
          (2, Other.vectorOfN(size, gen)),
          (5, Other.vectorOfN(size * 5, gen)),
          (1, Other.vectorOfN(1, gen))
        ).map(_.take(1000))
    )

    for {
        majorVersion <- Gen.posNum[Int]

        genDeposit = genDepositUtxo(
          config = config,
          headAddress = Some(config.headMultisigAddress)
        )()
        deposits <- genHelper(genDeposit).map(_.take(config.maxDepositsAbsorbedPerBlock))

        payouts <- genHelper(genPayoutObligation(config.cardanoNetwork))
        payoutAda = payouts
            .map(_.utxo.value.value.coin)
            .fold(Coin.zero)(_ + _)

        kzg: KzgCommitment <- kzgCommitment match {
            case None =>
                Gen.listOfN(48, Arbitrary.arbitrary[Byte])
                    .map(list => ByteString.fromArray(list.toArray))
            case Some(kzg) => Gen.const(kzg)
        }

        multisigTreasury <- for {
            utxoId <- Arbitrary.arbitrary[TransactionInput]

            equity <- Gen.choose(100_000_000L, 10_000_000_000L).map(l => Equity(Coin(l)).get)
            treasuryValue = payoutAda + equity.coin
        } yield MultisigTreasuryUtxo(
          treasuryTokenName = config.headTokenNames.treasuryTokenName,
          utxoId = utxoId,
          address = config.headMultisigAddress,
          datum = MultisigTreasuryUtxo.Datum(kzg, majorVersion),
          value = Value(treasuryValue),
          equity = equity
        )

    } yield SettlementTxSeq.Build(config)(
      kzgCommitment = kzg,
      majorVersionProduced = BlockVersion.Major(majorVersion),
      depositsToSpend = deposits.toList,
      payoutObligationsRemaining = payouts,
      treasuryToSpend = multisigTreasury,
      competingFallbackValidityStart = fallbackTxStartTime,
      blockCreationEndTime = blockCreationEndTime
    )
}

/** Generates the settlement seq builder for the next settlement tx that should correctly spend the
  * [[treasuryToSpend]].
  * @param treasuryToSpend
  *   the treasury to be spent in the settlement sequence
  * @param majorVersion
  *   the version of the next block
  */
def genNextSettlementTxSeqBuilder(config: HeadConfig)(
    treasuryToSpend: MultisigTreasuryUtxo,
    fallbackValidityStart: FallbackTxStartTime,
    blockCreationEndTime: BlockCreationEndTime,
    majorVersion: Int,
    estimatedFee: Coin = Coin(5_000_000L),
    // If passed, the kzg commitment will be set to the value.
    // If not, its randomly generated
    kzgCommitment: Option[KzgCommitment] = None
): Gen[SettlementTxSeq.Build] = {
    // A helper to generator empty, small, medium, large (up to 1000)
    def genHelper[T](gen: Gen[T]): Gen[Vector[T]] = Gen.sized(size =>
        Gen.frequency(
          (1, Gen.const(Vector.empty)),
          (2, Other.vectorOfN(size, gen)),
          (5, Other.vectorOfN(size * 5, gen)),
          (1, Other.vectorOfN(1, gen))
        ).map(_.take(1000))
    )

    val genDeposit = genDepositUtxo(
      config,
      headAddress = Some(config.headMultisigAddress)
    )()

    given Ord[v1.Value] = valueOrd

    for {
        deposits <- genHelper(genDeposit)
        payouts <- genHelper(genPayoutObligation(config.cardanoNetwork))
        prefixes = (payouts.length to 0 by -1).map(payouts.take)
        infimum = prefixes
            .find(prefix =>
                getValue(
                  prefix
                      .map(_.utxo.value.value)
                      .fold(Value.zero)(_ + _)
                ) <= getValue(treasuryToSpend.value)
            )
            // Since we have empty prefix that always satisfies the condition this is safe
            .get
        kzg: KzgCommitment <- kzgCommitment match {
            case None =>
                Gen.listOfN(48, Arbitrary.arbitrary[Byte])
                    .map(list => ByteString.fromArray(list.toArray))
            case Some(kzg) => Gen.const(kzg)
        }
    } yield (
      SettlementTxSeq.Build(config)(
        kzgCommitment = kzg,
        majorVersionProduced = BlockVersion.Major(majorVersion),
        depositsToSpend = deposits.toList,
        payoutObligationsRemaining = infimum,
        treasuryToSpend = treasuryToSpend,
        competingFallbackValidityStart = fallbackValidityStart,
        blockCreationEndTime = blockCreationEndTime
      ),
    )
}
