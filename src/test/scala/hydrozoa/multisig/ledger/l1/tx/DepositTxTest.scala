package hydrozoa.multisig.ledger.l1.tx

import cats.data.NonEmptyList
import hydrozoa.config.head.multisig.timing.TxTiming.RequestTimes.unsafeRequestValidityEndTime
import hydrozoa.config.node.{MultiNodeConfig, NodeConfig}
import hydrozoa.lib.cardano.scalus.given_Choose_Coin
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.eutxol2.tx.GenesisObligation
import hydrozoa.multisig.ledger.l1.tx.Metadata as MD
import hydrozoa.multisig.ledger.l1.utxo.DepositUtxo
import java.util.concurrent.TimeUnit
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.propBoolean
import org.scalacheck.{Arbitrary, Gen, Prop, Properties}
import scala.concurrent.duration.FiniteDuration
import scalus.cardano.ledger.ArbitraryInstances.given
import scalus.cardano.ledger.{Hash, *}
import scalus.cardano.onchain.plutus.v3.ArbitraryInstances.*
import scalus.uplc.builtin.ByteString
import test.*
import test.Generators.Hydrozoa.*

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
                unsafeRequestValidityEndTime(
                  config.initialBlock.endTime + FiniteDuration(sec, TimeUnit.SECONDS)
                )
            )

        l2Addr <- genPubkeyAddress(config)
        refundAddr <- genPubkeyAddress(config)

        instructions =
            DepositUtxo.Refund.Instructions(
              address = refundAddr,
              datum = refundData,
              validityStart = config.txTiming.refundValidityStart(requestValidityEndTime)
            )

        txId <- arbitrary[TransactionInput]

        depositorAddress <- multiNodeConfig.pickPeer.map(multiNodeConfig.addressOf)

        l2Outputs <- Gen
            .nonEmptyListOf(
              genGenesisObligation(config, depositorAddress, minimumCoin = Coin.ada(2))
            )
            .map(NonEmptyList.fromListUnsafe)

        depositFee <- Gen.frequency(
          5 -> Gen.const(Coin.zero),
          5 -> Gen.choose(Coin(500_000), Coin(5_000_000))
        )

        l2Value = Value.combine(l2Outputs.map(vo => Value(vo.l2OutputValue)).toList)
        depositValue = l2Value + Value(depositFee)

        // TODO: use arbitrary values, not just ADA only
        fundingUtxos <- Gen
            .nonEmptyListOf(genAdaOnlyPubKeyUtxo(config, depositorAddress))
            .map(NonEmptyList.fromListUnsafe)
            // FIXME: suchThat wastes a lot of generation time
            // TODO: Use Hydrozoa's Value once tokens are added
            .suchThat(utxos =>
                Value.combine(utxos.toList.map(_.output.value)).coin > depositValue.coin
            )

        refundAddr <- genPubkeyAddress(config)

    } yield DepositTx.Build(config)(
      utxosFunding = fundingUtxos,
      l2Payload = GenesisObligation.serialize(l2Outputs),
      depositFee = depositFee,
      changeAddress = depositorAddress,
      requestValidityEndTime = requestValidityEndTime,
      refundInstructions = instructions,
      l2Value = l2Value
    )
}

object DepositTxTest extends Properties("Deposit Tx Test") {

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
                    case Left(e) => "Build failed" |: Prop(false)
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
