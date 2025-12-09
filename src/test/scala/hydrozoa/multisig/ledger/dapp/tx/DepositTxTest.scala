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
import scalus.cardano.txbuilder.TransactionBuilder.ensureMinAda
import scalus.prelude.Option as SOption
import test.*
import test.Generators.Hydrozoa.*

var counter = AtomicLong(0L)

def genDepositRecipe(
    estimatedFee: Coin = Coin(5_000_000L),
    params: ProtocolParams = blockfrost544Params
): Gen[DepositTx.Recipe] =
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
          address = LedgerToPlutusTranslation.getAddress(l2Addr).credential,
          datum = depositData,
          deadline = deadline,
          refundAddress = LedgerToPlutusTranslation.getAddress(refundAddr),
          refundDatum = refundData
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

        depositAmount <- Gen.posNum[Long].map(n => Coin(n) + depositMinAda)

        // TODO: use arbitrary values, not just ADA only
        fundingUtxos <- Gen
            .nonEmptyListOf(genAdaOnlyPubKeyUtxo(depositor))
            .map(NonEmptyList.fromListUnsafe)
            .suchThat(utxos =>
                sumUtxoValues(utxos.toList).coin > minPubkeyAda() + depositAmount + estimatedFee
            )

    } yield DepositTx.Recipe(
      depositAmount = depositAmount,
      datum = depositDatum,
      headAddress = headAddress,
      utxosFunding = fundingUtxos,
      changeAddress = depositor.address(testNetwork),
      network = testNetwork,
      protocolParams = testProtocolParams,
      evaluator = testEvaluator,
      validators = nonSigningValidators
    )

class DepositTxTest extends AnyFunSuite with ScalaCheckPropertyChecks {

    implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
        PropertyCheckConfiguration(minSuccessful = 100)

    given pv: ProtocolVersion = ProtocolVersion.conwayPV

    // override def scalaCheckInitialSeed = "SfYvj1tuRnXN2LkzQzKEbLA6LEPVYNSFj2985MfH0ZO="

    test("Roundtrip deposit metadata") {
        forAll(genScriptAddress()) { addr =>
            val mbAux = Some(KeepRaw(MD(MD.Deposit(addr))))
            MD.parse(mbAux) match {
                case Right(_) => ()
                case Left(e)  => fail(e.toString)
            }
        }
    }

    test("Build deposit tx") {
        forAll(genDepositRecipe()) { recipe =>
            DepositTx.build(recipe) match {
                case Left(e) => fail(s"Build failed $e")
                case Right(tx) =>
                    DepositTx.parse(tx.tx.toCbor) match {
                        case Left(e) =>
                            fail(
                              s"Produced deposit tx cannot be deserialized from CBOR: ${e.getCause}"
                            )
                        case Right(cborParsed) if cborParsed != tx =>
                            // println(ByteString.fromArray(tx.tx.toCbor).toHex)
                            // assert(expected = tx.tx.body.value.outputs(1), obtained = cborParsed.tx.body.value.outputs(1))
                            assertResult(tx)(cborParsed)
                        case _ => ()
                    }
            }
        }
    }
}
