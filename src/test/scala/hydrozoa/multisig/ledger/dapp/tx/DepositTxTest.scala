package hydrozoa.multisig.ledger.dapp.tx

import cats.data.NonEmptyList
import cats.effect.unsafe.implicits.global
import hydrozoa.config.HeadConfig.Fields.{HasCardanoInfo, HasEvaluator, HasHeadAddress, HasHeadMultisigScript, HasMultisigRegimeUtxo, HasTokenNames, HasTxTiming}
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant.realTimeQuantizedInstant
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.token.CIP67.TokenNames
import hydrozoa.multisig.ledger.dapp.tx.Metadata as MD
import hydrozoa.multisig.ledger.dapp.utxo.DepositUtxo
import java.util.concurrent.atomic.AtomicLong
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.builtin.Data.toData
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.*
import scalus.cardano.ledger.ArbitraryInstances.given
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.rules.STS
import scalus.cardano.txbuilder.TransactionBuilder
import scalus.cardano.txbuilder.TransactionBuilder.ensureMinAda
import scalus.prelude.Option as SOption
import scalus.testing.kit.TestUtil.{genByteStringOfN, testEnvironment}
import test.*
import test.Generators.Hydrozoa.*
import test.TestPeer.Alice

var counter = AtomicLong(0L)

def genDepositRecipe(
    estimatedFee: Coin = Coin(5_000_000L),
    params: ProtocolParams = blockfrost544Params
): Gen[DepositTx.Builder] =
    for {
        depositor <- genTestPeer
        headAddress <- genScriptAddress()
        genData = Gen.frequency(
          (99, genByteStringData.map(data => SOption.Some(data))),
          (1, SOption.None)
        )
        depositData <- genData
        refundData <- genData
        deadline: BigInt <- Gen.posNum[BigInt]

        l2Addr <- genPubkeyAddress()
        refundAddr <- genPubkeyAddress()

        depositDatum = DepositUtxo.Datum(
          DepositUtxo.Refund.Instructions(
            address = LedgerToPlutusTranslation.getAddress(refundAddr),
            datum = refundData,
            startTime = QuantizedInstant(
              instant = java.time.Instant.ofEpochMilli(deadline.toLong),
              slotConfig = testTxBuilderEnvironment.slotConfig
            )
          )
        )

        seedUtxo <- arbitrary[TransactionInput]

        depositMinAda = {
            val candidate = Babbage(
              address = headAddress,
              value = Value.zero,
              datumOption = Some(Inline(depositDatum.toData)),
              scriptRef = None
            )
            ensureMinAda(candidate, params).value.coin
        }

        virtualOutputs <- Gen
            .nonEmptyListOf(genGenesisObligation(Alice, minimumCoin = Coin.ada(2)))
            .map(NonEmptyList.fromListUnsafe)

        depositAmount = Value.combine(virtualOutputs.map(vo => Value(vo.l2OutputValue)).toList)

        // TODO: use arbitrary values, not just ADA only
        fundingUtxos <- Gen
            .nonEmptyListOf(genAdaOnlyPubKeyUtxo(depositor))
            .map(NonEmptyList.fromListUnsafe)
            // FIXME: suchThat wastes a lot of generation time
            .suchThat(utxos =>
                sumUtxoValues(
                  utxos.toList
                ).coin > minPubkeyAda() + depositAmount.coin + estimatedFee
            )

        txBuilderConfig: Tx.Builder.Config <- genTxConfig(
          testEnvironment,
          testEvaluator,
          nonSigningNonValidityChecksValidators
        )

        depositBuilderConfig = new HasTxTiming
            with HasHeadMultisigScript
            with HasMultisigRegimeUtxo
            with HasTokenNames
            with HasCardanoInfo
            with HasValidators
            with HasEvaluator
            with HasHeadAddress {
            val txTiming: TxTiming = TxTiming.default(txBuilderConfig.cardanoInfo.slotConfig)
            val headMultisigScript: HeadMultisigScript = txBuilderConfig.headMultisigScript
            val tokenNames: TokenNames = TokenNames(seedUtxo)
            val cardanoInfo: CardanoInfo = txBuilderConfig.cardanoInfo
            val validators: Seq[STS.Validator] = nonSigningNonValidityChecksValidators
            val evaluator: PlutusScriptEvaluator = txBuilderConfig.evaluator
            val hasEvaluator: ShelleyAddress = txBuilderConfig.headAddress
            def headAddress: scalus.cardano.address.ShelleyAddress = ???
            def multisigRegimeUtxo: hydrozoa.multisig.ledger.dapp.utxo.MultisigRegimeUtxo = ???
        }

        refundAddr <- genPubkeyAddress()

        partialRefundTx = RefundTx.Builder.PartialResult.PostDated(
          ctx = TransactionBuilder.Context.empty(testNetwork),
          inputValueNeeded = depositAmount,
          refundInstructions = DepositUtxo.Refund.Instructions(
            address = LedgerToPlutusTranslation.getAddress(refundAddr),
            datum = SOption.None,
            // TODO: move to propertyM
            startTime =
                realTimeQuantizedInstant(txBuilderConfig.cardanoInfo.slotConfig).unsafeRunSync()
          ),
          slotConfig = txBuilderConfig.cardanoInfo.slotConfig
        )

    } yield DepositTx.Builder(
      config = depositBuilderConfig,
      partialRefundTx = partialRefundTx,
      utxosFunding = fundingUtxos,
      virtualOutputs = virtualOutputs,
      donationToTreasury = Coin(0), // TODO: generate non-zero
      changeAddress = depositor.address(testNetwork),
    )

class DepositTxTest extends AnyFunSuite with ScalaCheckPropertyChecks {

    implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
        PropertyCheckConfiguration(minSuccessful = 100)

    given pv: ProtocolVersion = ProtocolVersion.conwayPV

    // override def scalaCheckInitialSeed = "SfYvj1tuRnXN2LkzQzKEbLA6LEPVYNSFj2985MfH0ZO="

    test("Roundtrip deposit metadata") {
        val gen =
            for {
                addr <- genScriptAddress()
                hash <- genByteStringOfN(32)
                index <- Gen.posNum[Int].map(_ - 1)
            } yield (addr, index, Hash[Blake2b_256, Any](hash))
        forAll(gen) { (addr, idx, hash) =>
            val aux = MD(MD.Deposit(addr, idx, hash))
            MD.parse(Some(KeepRaw(aux))) match {
                case Right(x: MD.Deposit) =>
                    x.headAddress == addr
                    && x.depositUtxoIx == idx
                    && x.virtualOutputsHash == hash
                case Right(_) => fail("not deposit metadata")
                case Left(e)  => fail(e.toString)
            }
        }
    }

    test("Build deposit tx") {
        forAll(genDepositRecipe()) { depositTxBuilder =>
            depositTxBuilder.build() match {
                case Left(e) => fail(s"Build failed $e")
                case Right(depositTx) =>
                    DepositTx.parse(
                      depositTx.tx.toCbor,
                      depositTxBuilder.config,
                      depositTx.depositProduced.virtualOutputs
                    ) match {
                        case Left(e) =>
                            fail(
                              s"Produced deposit tx cannot be deserialized from CBOR: ${e.getCause}"
                            )
                        case Right(cborParsed) if cborParsed != depositTx =>
                            // println(ByteString.fromArray(tx.tx.toCbor).toHex)
                            // assert(expected = tx.tx.body.value.outputs(1), obtained = cborParsed.tx.body.value.outputs(1))
                            assertResult(depositTx)(cborParsed)
                        case _ => ()
                    }
            }
        }
    }
}
