package hydrozoa.multisig.ledger.dapp.tx

//import cats.data.*
//import hydrozoa.*
//import hydrozoa.config.head.multisig.timing.TxTiming
//import hydrozoa.lib.cardano.scalus.QuantizedTime.{QuantizedInstant, quantize}
//import hydrozoa.multisig.ledger.block.BlockVersion
//import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
//import hydrozoa.multisig.ledger.dapp.token.CIP67
//import hydrozoa.multisig.ledger.dapp.txseq.FinalizationTxSeq
//import hydrozoa.multisig.ledger.dapp.utxo.{MultisigRegimeUtxo, MultisigTreasuryUtxo}
//import hydrozoa.multisig.ledger.virtual.commitment.KzgCommitment.KzgCommitment
//import hydrozoa.rulebased.ledger.dapp.tx.genEquityShares
//import java.time.Instant
//import org.scalacheck.Arbitrary.arbitrary
//import org.scalacheck.{Arbitrary, Gen}
//import scala.jdk.CollectionConverters.*
//import scalus.cardano.address.ShelleyDelegationPart.Null
//import scalus.cardano.address.{Network, ShelleyAddress, ShelleyPaymentPart}
//import scalus.cardano.ledger.*
//import scalus.cardano.ledger.ArbitraryInstances.given
//import scalus.cardano.txbuilder.TransactionBuilder.ensureMinAda
//import test.*
//import test.Generators.Hydrozoa.*
//import test.Generators.Other
//import test.TestPeer.mkWallet
//
//val genMultisigRegimeTokenName: Gen[AssetName] =
//    for {
//        ti <- arbitrary[TransactionInput]
//    } yield CIP67.HeadTokenNames(ti).multisigRegimeTokenName
//
//def genMultisigRegimeUtxo(
//    network: Network = testNetwork,
//    params: ProtocolParams = blockfrost544Params,
//    headAddress: Option[ShelleyAddress],
//    coin: Option[Coin],
//    script: HeadMultisigScript
//): Gen[MultisigRegimeUtxo] =
//    for {
//        txId <- arbitrary[TransactionInput]
//        multisigRegimeTn <- genMultisigRegimeTokenName
//
//        scriptAddress = headAddress.getOrElse {
//            ShelleyAddress(network, ShelleyPaymentPart.Script(genScriptHash.sample.get), Null)
//        }
//
//        multisigRegimeToken = singleton(
//          scriptAddress.payment.asInstanceOf[ShelleyPaymentPart.Script].hash,
//          multisigRegimeTn
//        )
//
//        treasuryMinAda = ensureMinAda(
//          MultisigRegimeUtxo(
//            multisigRegimeTokenName = multisigRegimeTn,
//            utxoId = txId,
//            address = scriptAddress,
//            value = Value(Coin(0L)) + multisigRegimeToken,
//            script = script
//          ).asUtxo._2,
//          params
//        ).value.coin
//
//        // TODO: Why?
//        treasuryAda <- arbitrary[Coin].map(l => l - Coin(1L) + treasuryMinAda)
//
//    } yield MultisigRegimeUtxo(
//      multisigRegimeTokenName = multisigRegimeTn,
//      utxoId = txId,
//      address = scriptAddress,
//      value = Value(coin.getOrElse(treasuryAda)) + multisigRegimeToken,
//      script = script
//    )
//
//def genStandaloneFinalizationTxSeqBuilder(
//    estimatedFee: Coin = Coin(5_000_000L),
//    params: ProtocolParams = blockfrost544Params,
//    network: Network = testNetwork,
//    // If passed, the kzg commitment will be set to the value.
//    // If not, its randomly generated
//    kzgCommitment: Option[KzgCommitment] = None
//): Gen[(FinalizationTxSeq.Builder, FinalizationTxSeq.Builder.Args, NonEmptyList[TestPeer])] = {
//    // A helper to generator empty, small, medium, large (up to 1000)
//    def genHelper[T](gen: Gen[T]): Gen[Vector[T]] = Gen.sized(size =>
//        Gen.frequency(
//          (1, Gen.const(Vector.empty)),
//          (2, Other.vectorOfN(size, gen)),
//          (5, Other.vectorOfN(size * 5, gen)),
//          (1, Other.vectorOfN(1, gen))
//        ).map(_.take(1000))
//    )
//
//    for {
//        peers <- genTestPeers()
//        hms = HeadMultisigScript(peers.map(mkWallet(_).exportVerificationKeyBytes))
//        mw <- genFakeMultisigWitnessUtxo(
//          hms,
//          testTxBuilderCardanoInfo.network
//        )
//
//        majorVersion <- Gen.posNum[Int]
//
//        payouts <- genHelper(genPayoutObligation(network))
//        payoutAda = payouts
//            .map(_.utxo.value.coin)
//            .fold(Coin.zero)(_ + _)
//
//        headAddress = hms.mkAddress(network)
//
//        treasuryUtxo <- genTreasuryUtxo(
//          headAddress = Some(headAddress),
//          network = network,
//          coin = Some(payoutAda + Coin(1_000_000_000L))
//        )
//
//        shares <- genEquityShares(peers)
//
//        kzg: KzgCommitment <- kzgCommitment match {
//            case None      => Gen.listOfN(48, Arbitrary.arbitrary[Byte]).map(IArray.from(_))
//            case Some(kzg) => Gen.const(kzg)
//        }
//    } yield (
//      FinalizationTxSeq.Builder(config =
//          FinalizationTxSeq.Config(testTxTiming, mw, testTxBuilderCardanoInfo, hms, shares)
//      ),
//      FinalizationTxSeq.Builder.Args(
//        majorVersionProduced = BlockVersion.Major(majorVersion),
//        treasuryToSpend = treasuryUtxo,
//        payoutObligationsRemaining = payouts,
//        competingFallbackValidityStart = Instant
//            .ofEpochMilli(System.currentTimeMillis() + 3_600_000)
//            .quantize(testTxBuilderCardanoInfo.slotConfig),
//        blockCreatedOn = Instant
//            .ofEpochMilli(System.currentTimeMillis())
//            .quantize(testTxBuilderCardanoInfo.slotConfig)
//      ),
//      peers
//    )
//}
//
//def genFinalizationTxSeqBuilder(
//    config: FinalizationTxSeq.Config,
//    treasuryToSpend: MultisigTreasuryUtxo,
//    majorVersion: Int,
//    fallbackValidityStart: QuantizedInstant,
//    blockCreatedOn: QuantizedInstant,
//    txTiming: TxTiming,
//    peers: NonEmptyList[TestPeer],
//    estimatedFeesAndEquity: Coin = Coin(50_000_000L),
//    params: ProtocolParams = blockfrost544Params,
//    network: Network = testNetwork,
//    // If passed, the kzg commitment will be set to the value.
//    // If not, its randomly generated
//    kzgCommitment: Option[KzgCommitment] = None
//): Gen[(FinalizationTxSeq.Builder, FinalizationTxSeq.Builder.Args)] = {
//
//    val payoutsTotal = treasuryToSpend.value.coin.value - estimatedFeesAndEquity.value
//
//    for {
//
//        coins <- Gen.tailRecM[List[Long], List[Long]](List.empty) { tails =>
//            val residual = payoutsTotal - tails.sum
//            if residual < 15_000_000
//            then Gen.const(Right(residual :: tails))
//            else
//                for next <- Gen.choose(5_000_000L, residual)
//                yield Left(next :: tails)
//        }
//
//        payouts <- Gen.sequence(coins.map(l => genKnownCoinPayoutObligation(network, Coin(l))))
//
//        kzg: KzgCommitment <- kzgCommitment match {
//            case None      => Gen.listOfN(48, Arbitrary.arbitrary[Byte]).map(IArray.from(_))
//            case Some(kzg) => Gen.const(kzg)
//        }
//    } yield (
//      FinalizationTxSeq.Builder(config = config),
//      FinalizationTxSeq.Builder.Args(
//        majorVersionProduced = BlockVersion.Major(majorVersion),
//        treasuryToSpend = treasuryToSpend,
//        payoutObligationsRemaining = payouts.asScala.toVector,
//        competingFallbackValidityStart = fallbackValidityStart,
//        blockCreatedOn = blockCreatedOn
//      )
//    )
//}
