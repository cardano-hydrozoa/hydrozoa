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
import org.scalacheck.{Gen, Prop, Properties, Test}
import scala.collection.mutable
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

/*
@Ilia (from Peter):
  Now that we have a "god config", the most efficient way of running these tests is going to be re-using
  the generated config at as high a (lexical) level as we can.

  I hoped to use this test as a demonstration, but I don't want to spend much time on this and it didn't work out
  quite as planned. Nonetheless:

  The first thing to note is that we _may_ want to do away with the
     object FooTest extends Properties("Foo")
  and just test a handful of mega-properties directly.

  The second thing to note is that `PropertyM` gives us a _much_ nicer syntax and reporting WRT to generated-vs-computed
  data, at the cost of giving up shrinking in the current implementation (but this could be fixed for limited cases
  -- though obviously not IO -- in the future).

  The basic idea is to:
    - Generate a _list_ of TestNodeConfig (say, of 10 elements)
    - For every TestNodeConfig:
       - For every property, generate a _list_ of property-specific test data (say, 100)
       - Build the properties inside

  This way, if we have 5 properties, we get 5,000 test cases off of 10 of the "expensive" generations.

  The way I did something similar in the old `InitializationTxSeqTest` was by making a `props = mutable.Buffer[Prop].empty`
  and populating it; I believe PropertyBuilder worked the same. PropertyM did it purely.

  Where I've left off with this:
    - I confirmed that the old tests did work(*) with the new configuration using the scalatest setup, at least
      with 100 test cases.
      - (*) When I tested with more cases, I did run into a bug where the initialization tx generator was generating
        too many funding/change utxos. I fixed it, but be advised that other bugs may linger
    - I refactored to use scalacheck instead of scalatest+.
    - Things compile, but it appears that test cases are showing up as discarded, and I'm not quite sure why.

   Open questions:
     - Without native support for this "tree-like" property test structure, I'm a little bit worried about what a failure
       will look like _without_ using PropertyM or PropertyBuilder -- we're generating a list of 100 samples,
       and a failure in one won't pinpoint which sample failed. Can we improve this?
     - Without PropertyM/PropertyBuilder, is there a way to having meaningful messages within, say, a case match?
       This is the functionality of `PropertyM.assertWith`, but you'll see that things that were assertions (exceptions)
       in the scalatest version just become booleans in the `Prop` version.
     - What does shrinking look like with this approach?
     - Why are tests getting discarded?

   Lastly, where to pick up: Realistically, I don't think we have time to do this refactor right now. I think the most
   important thing is to get the property tests running again so that we (a) can be more confident that the configuration
   refactor didn't introduce subtle bugs, (b) have harnesses ready for when we inevitably want to change some behavior
   going forward and need to protect against regressions. Thus, I would recommend taking a look at what I'm doing below,
   and unless fresh eyes lead you to a better solution, I'd recommend just reverting this to a

   ```
   val _ = property("metadata roundtrips") = ???
   val _ = property("deposit tx builds") = ???
   ```

   And moving on to restoring the other tests.

   I'll leave it to you to decide whether you want to restore intermediate "Minimal" configuration types to satisfy the
   type aliases in, e.g., `DepositTx.Config`, but personally I would just parameterize most things around TestNodeConfig
   (except, obviously, the inititalization tests that need to precede it) and byte the bullet on longer generation
   until we get to a refactor. Otherwise it seems like we will just end up exactly where we started, with each component
   having its own configuration type and generators.
 */
object DepositTxTest extends Properties("Deposit Tx Test") {
    override def overrideParameters(p: Test.Parameters): Test.Parameters = {
        // This is purely to speed up the tests while debugging
        p.withMinSuccessfulTests(1)
    }

    val _ = property("deposit tests") =
        // Generate the testNodeConfig `n` times once; n == 1 for now.
        Prop.forAll(Gen.listOfN(1, generateTestNodeConfig)) { testNodeConfigs =>
            // Buffer to collect all the properties. At the end we do props.fold(Prop.True)(_ && _ )
            val props: mutable.Buffer[Prop] = mutable.Buffer.empty

            testNodeConfigs.foreach { testNodeConfig =>
                val config = testNodeConfig.nodeConfig
                val gen =
                    for {
                        addr <- genScriptAddress(config)
                        hash <- genByteStringOfN(32)
                        index <- Gen.posNum[Int].map(_ - 1)
                    } yield (addr, index, Hash[Blake2b_256, Any](hash))

                props.append(
                  "Metadata round trips" |:
                      Prop.forAll(Gen.listOfN(100, gen))(_.forall((addr, idx, hash) =>
                          val aux = MD(MD.Deposit(addr, idx, hash))
                          MD.parse(Some(KeepRaw(aux)))(using config.cardanoProtocolVersion) match {
                              case Right(x: MD.Deposit) =>
                                  x.headAddress == addr
                                  && x.depositUtxoIx == idx
                                  && x.virtualOutputsHash == hash
                              case Right(_) => false
                              case Left(e)  => false
                          }
                      ))
                )

                props.append(
                  "Build deposit tx" |:
                      Prop.forAll(Gen.listOfN(100, genDepositRecipe(testNodeConfig)()))(
                        _.forall(depositTxBuilder =>
                            depositTxBuilder.result match {
                                // @Ilia: Perhaps this could be
                                //   props.append(Prop(false).label("deposit tx builds successfully)),
                                // and we remove the outer `props.append`?
                                // (likewise for the other failing branches)
                                case Left(e) => false // (s"Build failed $e")
                                case Right(depositTx) =>
                                    DepositTx
                                        .Parse(config)(
                                          depositTx.tx.toCbor,
                                          depositTx.depositProduced.virtualOutputs
                                        )
                                        .result match {
                                        case Left(e) =>
                                            false // s"Produced deposit tx cannot be deserialized from CBOR: ${e.getCause}"

                                        case Right(cborParsed) if cborParsed != depositTx =>
                                            // println(ByteString.fromArray(tx.tx.toCbor).toHex)
                                            // assert(expected = tx.tx.body.value.outputs(1), obtained = cborParsed.tx.body.value.outputs(1))
                                            false
                                        case _ => true
                                    }
                            }
                        )
                      )
                )
            }
            props.fold(Prop(true))(_ && _)
        }

}
