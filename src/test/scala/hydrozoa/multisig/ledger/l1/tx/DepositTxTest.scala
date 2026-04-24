package hydrozoa.multisig.ledger.l1.tx

import cats.data.NonEmptyList
import hydrozoa.config.head.multisig.timing.TxTiming.RequestTimes.*
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.lib.cardano.scalus.given_Choose_Coin
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.eutxol2.tx.GenesisObligation
import hydrozoa.multisig.ledger.l1.tx.Metadata as MD
import hydrozoa.multisig.ledger.l1.utxo.DepositUtxo
import java.util.concurrent.TimeUnit
import monocle.*
import monocle.syntax.all.*
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.propBoolean
import org.scalacheck.{Arbitrary, Gen, Prop, Properties}
import scala.concurrent.duration.FiniteDuration
import scalus.cardano.ledger.ArbitraryInstances.given
import scalus.cardano.ledger.TransactionOutput.valueLens
import scalus.cardano.ledger.{Hash, *}
import scalus.cardano.onchain.plutus.v3.ArbitraryInstances.*
import scalus.uplc.builtin.ByteString
import test.*
import test.Generators.Hydrozoa.*
import test.Generators.Other.genValueDistribution

def genDepositBuilder(multiNodeConfig: MultiNodeConfig): Gen[DepositTx.Build] = {

    val config = multiNodeConfig.nodeConfigs(HeadPeerNumber.zero)

    for {
        headAddress <- Gen.const(config.headMultisigAddress)

        genData = Gen.frequency(
          (99, genByteStringData.map(data => Some(data))),
          (1, None)
        )
        depositData <- genData
        refundData <- genData

        requestValidityEndTime <- Gen
            .posNum[Long]
            .map(sec =>
                RequestValidityEndTime(
                  config.initialBlock.endTime + FiniteDuration(sec, TimeUnit.SECONDS)
                )
            )

        l2Addr <- genPubkeyAddress()(using config)
        refundAddr <- genPubkeyAddress()(using config)

        instructions =
            DepositUtxo.Refund.Instructions(
              address = refundAddr,
              datum = refundData,
              validityStart = config.txTiming.refundValidityStart(requestValidityEndTime)
            )

        txId <- arbitrary[TransactionInput]

        depositorAddress <- multiNodeConfig.pickPeer.map(multiNodeConfig.addressOf)

        nL2Outputs <- Gen.choose(1, 10)
        l2Outputs <- Gen
            .listOfN(
              nL2Outputs,
              genGenesisObligation(
                depositorAddress,
                genValue = genPositiveValue
              )(using config)
            )
            .map(NonEmptyList.fromListUnsafe)

        depositFee <- Gen.frequency(
          5 -> Gen.const(Coin.zero),
          5 -> Gen.choose(Coin(500_000), Coin(5_000_000))
        )

        l2Value = Value.combine(l2Outputs.map(_.l2OutputValue).toList)
        depositValue = l2Value + Value(depositFee)

        fundingUtxos <-
            for {
                nFunding <- Gen.choose(1, 6)

                // Strategy: First gen arbitrary utxos with min ada, then add in at least as much as is needed to support
                // the deposit value distribution
                minFunding <- Gen
                    .listOfN(
                      nFunding,
                      genPubKeyUtxo(depositorAddress, Gen.const(Value.ada(5)))(using config)
                    )
                    .map(l => NonEmptyList.fromListUnsafe(l.take(3)))
                distribution <- genValueDistribution(depositValue, minFunding.length)
                zipped: NonEmptyList[(Utxo, Value)] = minFunding.zip(distribution)
            } yield zipped.map((utxo, additionalValue) =>
                utxo.focus(_.output).andThen(valueLens).modify(_ + additionalValue)
            )

        refundAddr <- genPubkeyAddress()(using config)

    } yield DepositTx.Build(
      utxosFunding = fundingUtxos,
      l2Payload = GenesisObligation.serialize(l2Outputs),
      depositFee = depositFee,
      changeAddress = depositorAddress,
      requestValidityEndTime = requestValidityEndTime,
      refundInstructions = instructions,
      l2Value = l2Value
    )(using config)
}

object DepositTxTest extends Properties("Deposit Tx Test") {
//    override def overrideParameters(p: Test.Parameters): Test.Parameters =
//        p.withInitialSeed(Seed.fromBase64("acCC2RZZ0k_j5emHOUqcuSC0RUDo1QkzDWHURe4HRjD=").get)

    val _ = property("Metadata can be parsed") =
        Prop.forAll(MultiNodeConfig.generate(TestPeersSpec.default)()) { multiNodeConfig =>
            val config = multiNodeConfig.nodeConfigs(HeadPeerNumber.zero)
            val gen = for {
                hash <- genByteStringOfN(32)
                index <- Gen.posNum[Int].map(_ - 1)
                fee <- Gen.choose(0, 100_000_000).map(Coin(_))
            } yield (index, Hash[Blake2b_256, Any](hash), fee)

            Prop.forAll(gen)((idx, hash, fee) =>
                val aux: AuxiliaryData.Metadata =
                    AuxiliaryData.Metadata(
                      MD.Deposit(idx, fee, hash).asAuxData(config.headId).getMetadata
                    )
                val expectedX = MD.Deposit(idx, fee, hash)

                MD.Deposit.parse(aux) match {
                    case Right(headId, x) if x.isInstanceOf[MD.Deposit] =>
                        "Metadata is as expected" |: (x == expectedX)
                    case Right(_) => "Metadata is MD.deposit" |: Prop(false)
                    case Left(e)  => "Metadata parsing returns Right" |: Prop(false)
                }
            )
        }

    val _ = property("Build deposit tx") =
        Prop.forAll(MultiNodeConfig.generate(TestPeersSpec.default)()) { multiNodeConfig =>
            val config = multiNodeConfig.nodeConfigs(HeadPeerNumber.zero)
            Prop.forAll(genDepositBuilder(multiNodeConfig))(depositBuilder =>
                depositBuilder.result match {
                    case Left(e) => s"Build failed: $e" |: Prop(false)
                    case Right(depositTx) =>
                        DepositTx
                            .Parse(config)(
                              ByteString.fromArray(depositTx.tx.toCbor),
                              depositTx.depositProduced.l2Payload,
                              depositTx.depositProduced.requestValidityEndTime
                            )
                            .result match {
                            case Left(e) =>
                                s"Produced deposit tx deserializes from CBOR: ${e.getMessage}"
                                    |: Prop(false)

                            case Right(cborParsed) if cborParsed != depositTx =>
                                "Parsed cbor round-trips" |: Prop(false)
                            case _ => Prop(true)
                        }
                }
            )
        }
}
