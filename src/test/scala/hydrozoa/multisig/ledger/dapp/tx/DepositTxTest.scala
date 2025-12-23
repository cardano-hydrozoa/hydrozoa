package hydrozoa.multisig.ledger.dapp.tx

import cats.data.NonEmptyList
import hydrozoa.multisig.ledger.dapp.tx.Metadata as MD
import hydrozoa.multisig.ledger.dapp.utxo.DepositUtxo
import java.util.concurrent.atomic.AtomicLong
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.builtin.Data.toData
import scalus.cardano.ledger.*
import scalus.cardano.ledger.ArbitraryInstances.given
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
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
            startTime = deadline
          )
        )

        txId <- arbitrary[TransactionInput]

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

        config: Tx.Builder.Config <- genTxConfig(
          testEnvironment,
          testEvaluator,
          nonSigningValidators
        )

        refundAddr <- genPubkeyAddress()

        partialRefundTx = RefundTx.Builder.PartialResult.PostDated(
          ctx = TransactionBuilder.Context.empty(testNetwork),
          inputValueNeeded = depositAmount,
          refundInstructions = DepositUtxo.Refund.Instructions(
            address = LedgerToPlutusTranslation.getAddress(refundAddr),
            datum = SOption.None,
            startTime = 0
          )
        )

    } yield DepositTx.Builder(
      config = config,
      partialRefundTx = partialRefundTx,
      utxosFunding = fundingUtxos,
      virtualOutputs = virtualOutputs,
      donationToTreasury = Coin(0), // TODO: generate non-zero
      changeAddress = depositor.address(testNetwork)
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
