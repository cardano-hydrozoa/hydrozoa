package hydrozoa.multisig.ledger.dapp.tx

import cats.data.NonEmptyList
import cats.effect.unsafe.implicits.global
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.network.CardanoNetwork.ensureMinAda
import hydrozoa.config.node.TestNodeConfig.generateTestNodeConfig
import hydrozoa.config.node.{NodeConfig, TestNodeConfig}
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant.realTimeQuantizedInstant
import hydrozoa.lib.number.PositiveInt
import hydrozoa.multisig.ledger.dapp.tx.Metadata as MD
import hydrozoa.multisig.ledger.dapp.utxo.DepositUtxo
import java.util.concurrent.atomic.AtomicLong
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.propBoolean
import org.scalacheck.{Gen, Prop, Properties}
import scalus.builtin.Data.toData
import scalus.cardano.ledger.ArbitraryInstances.given
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.{Hash, *}
import scalus.cardano.txbuilder.TransactionBuilder
import scalus.prelude.Option as SOption
import scalus.testing.kit.TestUtil.genByteStringOfN
import test.*
import test.Generators.Hydrozoa.*
import test.TestPeer.Alice

var counter = AtomicLong(0L)

def genDepositRecipe(testNodeConfig: TestNodeConfig)(
    estimatedFee: Coin = Coin(5_000_000L),
): Gen[DepositTx.Build] = {
    val testPeers = testNodeConfig.testPeers
    val config = testNodeConfig.nodeConfig
    for {
        depositor <- Gen.oneOf(testPeers._testPeers.toList.map(_._2))

        headAddress = config.headMultisigAddress
        genData = Gen.frequency(
          (99, genByteStringData.map(data => SOption.Some(data))),
          (1, SOption.None)
        )
        depositData <- genData
        refundData <- genData
        deadline: BigInt <- Gen.posNum[BigInt]

        l2Addr <- genPubkeyAddress(config)
        refundAddr <- genPubkeyAddress(config)

        depositDatum = DepositUtxo.Datum(
          DepositUtxo.Refund.Instructions(
            address = LedgerToPlutusTranslation.getAddress(refundAddr),
            datum = refundData,
            startTime = QuantizedInstant(
              instant = java.time.Instant.ofEpochMilli(deadline.toLong),
              slotConfig = config.slotConfig
            )
          )
        )

        txId <- arbitrary[TransactionInput]

        depositMinAda = {
            val candidate = Babbage(
              address = headAddress,
              value = Value.zero,
              datumOption = Some(Inline(depositDatum.toData)),
              scriptRef = None
            ).ensureMinAda(config)
            candidate.value.coin
        }

        virtualOutputs <- Gen
            .nonEmptyListOf(genGenesisObligation(config, Alice, minimumCoin = Coin.ada(2)))
            .map(NonEmptyList.fromListUnsafe)

        depositAmount = Value.combine(virtualOutputs.map(vo => Value(vo.l2OutputValue)).toList)

        minAda = config.babbageUtxoMinLovelace(PositiveInt.unsafeApply(200))

        // TODO: use arbitrary values, not just ADA only
        fundingUtxos <- Gen
            .nonEmptyListOf(genAdaOnlyPubKeyUtxo(config, depositor))
            .map(NonEmptyList.fromListUnsafe)
            // FIXME: suchThat wastes a lot of generation time
            .suchThat(utxos =>
                utxos.iterator
                    .map(_.output.value)
                    .foldLeft(Value.zero)(_ + _)
                    .coin > minAda + depositAmount.coin + estimatedFee
            )

        refundAddr <- genPubkeyAddress(config)

        partialRefundTx = RefundTx.PartialResult.PostDated(
          ctx = TransactionBuilder.Context.empty(config.network),
          inputValueNeeded = depositAmount,
          refundInstructions = DepositUtxo.Refund.Instructions(
            address = LedgerToPlutusTranslation.getAddress(refundAddr),
            datum = SOption.None,
            // TODO: move to propertyM
            startTime = realTimeQuantizedInstant(config.slotConfig).unsafeRunSync()
          ),
          slotConfig = config.slotConfig
        )

    } yield DepositTx.Build(config)(
      partialRefundTx = partialRefundTx,
      utxosFunding = fundingUtxos,
      virtualOutputs = virtualOutputs,
      donationToTreasury = Coin(0), // TODO: generate non-zero
      changeAddress = depositor.address(config.network),
    )
}

object DepositTxTest extends Properties("Deposit Tx Test") {

    val _ = property("deposit tests meta data parses") = Prop.forAll(generateTestNodeConfig) {
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
        Prop.forAll(genDepositRecipe(testNodeConfig)())(depositTxBuilder =>
            depositTxBuilder.result match {
                case Left(e) => "Build succeeds" |: Prop(false)
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
