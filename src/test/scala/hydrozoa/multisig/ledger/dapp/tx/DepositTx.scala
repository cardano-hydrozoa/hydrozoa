package hydrozoa.multisig.ledger.dapp.tx

import cats.data.NonEmptyList
import hydrozoa.lib.cardano.scalus.ledger.txbuilder.setMinAda
import hydrozoa.multisig.ledger.dapp.utxo.DepositUtxo
import org.scalacheck.{Test as ScalaCheckTest, *}
import scalus.builtin.Data.toData
import scalus.cardano.ledger.*
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.ledger.babbage.ProtocolParams
import scalus.prelude.Option as SOption
import test.*

import java.util.concurrent.atomic.AtomicLong

var counter = AtomicLong(0L)

def genDepositRecipe(
    estimatedFee: Coin = Coin(5_000_000L),
    params: ProtocolParams = blockfrost544Params
): Gen[DepositTx.Recipe] =
    for {
        depositor <- genTestPeer
        _ = println(counter.incrementAndGet())
        headAddress <- genScriptAddr()
        genData = Gen.frequency(
          (99, genByteStringData.map(data => SOption.Some((data)))),
          (1, SOption.None)
        )
        depositData <- genData
        refundData <- genData
        deadline: BigInt <- Gen.posNum[BigInt]

        l2Addr <- genPubkeyAddr()
        refundAddr <- genPubkeyAddr()

        depositDatum = DepositUtxo.Datum(
          address = (LedgerToPlutusTranslation.getAddress(l2Addr).credential),
          datum = depositData,
          deadline = deadline,
          refundAddress = LedgerToPlutusTranslation.getAddress(refundAddr),
          refundDatum = refundData
        )

        txId <- genTxId

        depositMinAda = {
            val candidate = Babbage(
              address = headAddress,
              value = Value.zero,
              datumOption = Some(Inline(depositDatum.toData)),
              scriptRef = None
            )
            setMinAda(candidate, params).value.coin
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
      changeAddress = depositor.address,
      context = unsignedTxBuilderContext(Map.from(fundingUtxos.toList))
    )

class DepositTxTest extends munit.ScalaCheckSuite {
    override def scalaCheckTestParameters: ScalaCheckTest.Parameters = {
        ScalaCheckTest.Parameters.default.withMinSuccessfulTests(10_000)
    }

    // override def scalaCheckInitialSeed = "SfYvj1tuRnXN2LkzQzKEbLA6LEPVYNSFj2985MfH0ZO="

    property("Build deposit tx")(
      Prop.forAll(genDepositRecipe()) { recipe =>
          DepositTx.build(recipe) match {
              case Left(e) => throw RuntimeException(s"Build failed $e")
              case Right(tx) =>
                  DepositTx.parse(tx.tx.toCbor) match {
                      case Left(e) =>
                          throw RuntimeException(
                            s"Produced deposit tx cannot be deserialized from CBOR"
                          )
                      case Right(cborParsed) if cborParsed != tx =>
                          // println(ByteString.fromArray(tx.tx.toCbor).toHex)
                          // assertEquals(expected = tx.tx.body.value.outputs(1), obtained = cborParsed.tx.body.value.outputs(1))
                          assertEquals(tx, cborParsed)
                      case _ => ()
                  }
          }
      }
    )
}
