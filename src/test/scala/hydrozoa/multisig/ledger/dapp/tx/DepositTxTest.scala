package hydrozoa.multisig.ledger.dapp.tx

import cats.data.NonEmptyList
import hydrozoa.config.node.TestNodeConfig.generateTestNodeConfig
import hydrozoa.config.node.{NodeConfig, TestNodeConfig}
import hydrozoa.lib.cardano.scalus.given_Choose_Coin
import hydrozoa.multisig.ledger.dapp.tx.Metadata as MD
import hydrozoa.multisig.ledger.dapp.utxo.DepositUtxo
import java.util.concurrent.TimeUnit
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.propBoolean
import org.scalacheck.{Gen, Prop, Properties}
import scala.concurrent.duration.FiniteDuration
import scalus.cardano.ledger.ArbitraryInstances.given
import scalus.cardano.ledger.{Hash, *}
import scalus.cardano.onchain.plutus.v3.ArbitraryInstances.*
import test.*
import test.Generators.Hydrozoa.*
import test.TestPeer.Alice

def genDepositBuilder(testNodeConfig: TestNodeConfig): Gen[DepositTx.Build] = {
    val testPeers = testNodeConfig.testPeers
    val config = testNodeConfig.nodeConfig
    for {
        depositor <- Gen.oneOf(testPeers._testPeers.toList.map(_._2))
        headAddress = config.headMultisigAddress
        genData = Gen.frequency(
          (99, genByteStringData.map(data => Some(data))),
          (1, None)
        )
        depositData <- genData
        refundData <- genData

        submissionDeadline <- Gen
            .posNum[Long]
            .map(sec => config.headStartTime + FiniteDuration(sec, TimeUnit.SECONDS))

        l2Addr <- genPubkeyAddress(config)
        refundAddr <- genPubkeyAddress(config)

        instructions =
            DepositUtxo.Refund.Instructions(
              address = refundAddr,
              datum = refundData,
              validityStart = config.txTiming.refundValidityStart(submissionDeadline)
            )

        txId <- arbitrary[TransactionInput]

        virtualOutputs <- Gen
            .nonEmptyListOf(genGenesisObligation(config, Alice, minimumCoin = Coin.ada(2)))
            .map(NonEmptyList.fromListUnsafe)

        depositFee <- Gen.frequency(
          5 -> Gen.const(Coin.zero),
          5 -> Gen.choose(Coin(500_000), Coin(5_000_000))
        )

        virtualValue = Value.combine(virtualOutputs.map(vo => Value(vo.l2OutputValue)).toList)
        depositValue = virtualValue + Value(depositFee)

        // TODO: use arbitrary values, not just ADA only
        fundingUtxos <- Gen
            .nonEmptyListOf(genAdaOnlyPubKeyUtxo(config, depositor))
            .map(NonEmptyList.fromListUnsafe)
            // FIXME: suchThat wastes a lot of generation time
            // TODO: Use Hydrozoa's Value once tokens are added
            .suchThat(utxos =>
                Value.combine(utxos.toList.map(_.output.value)).coin > depositValue.coin
            )

        refundAddr <- genPubkeyAddress(config)

    } yield DepositTx.Build(config)(
      utxosFunding = fundingUtxos,
      virtualOutputs = virtualOutputs,
      depositFee = depositFee,
      changeAddress = depositor.address(config.network),
      submissionDeadline = submissionDeadline,
      refundInstructions = instructions
    )
}

object DepositTxTest extends Properties("Deposit Tx Test") {

    val _ = property("Metadata can be parsed") = Prop.forAll(generateTestNodeConfig) {
        testNodeConfig =>
            val config = testNodeConfig.nodeConfig
            val gen = for {
                addr <- genScriptAddress(config)
                hash <- genByteStringOfN(32)
                index <- Gen.posNum[Int].map(_ - 1)
            } yield (addr, index, Hash[Blake2b_256, Any](hash))

            Prop.forAll(gen)((addr, idx, hash) =>
                val aux = MD(MD.Deposit(addr, idx, hash))
                MD.parse(Some(KeepRaw(aux)))(using config.cardanoProtocolVersion) match {
                    case Right(x: MD.Deposit) =>
                        "Metadata is as expected" |: (x.headAddress == addr
                            && x.depositUtxoIx == idx
                            && x.virtualOutputsHash == hash)
                    case Right(_) => "Metadata is MD.deposit" |: Prop(false)
                    case Left(e)  => "Metadata parsing returns Right" |: Prop(false)
                }
            )
    }

    val _ = property("Build deposit tx") = Prop.forAll(generateTestNodeConfig) { testNodeConfig =>
        val config = testNodeConfig.nodeConfig
        Prop.forAll(genDepositBuilder(testNodeConfig))(depositBuilder =>
            depositBuilder.result match {
                case Left(e) => "Build failed" |: Prop(false)
                case Right(depositTx) =>
                    DepositTx
                        .Parse(config)(
                          depositTx.tx.toCbor,
                          depositTx.depositProduced.virtualOutputs
                        )
                        .result match {
                        case Left(e) =>
                            "Produced deposit tx deserializes from CBOR: ${e.getCause}"
                                |: Prop(false)

                        case Right(cborParsed) if cborParsed != depositTx =>
                            "Parsed cbor round-trips" |: Prop(false)
                        case _ => Prop(true)
                    }
            }
        )
    }
}
