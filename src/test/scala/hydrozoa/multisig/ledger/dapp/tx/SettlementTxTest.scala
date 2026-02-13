package hydrozoa.multisig.ledger.dapp.tx

//import cats.data.*
//import hydrozoa.config.head.multisig.timing.TxTiming
//import hydrozoa.lib.cardano.scalus.QuantizedTime.{QuantizedInstant, quantize}
//import hydrozoa.multisig.ledger.block.BlockVersion
//import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
//import hydrozoa.multisig.ledger.dapp.token.CIP67.HeadTokenNames
//import hydrozoa.multisig.ledger.dapp.txseq.SettlementTxSeq
//import hydrozoa.multisig.ledger.dapp.utxo.{DepositUtxo, MultisigTreasuryUtxo}
//import hydrozoa.multisig.ledger.virtual.commitment.KzgCommitment.KzgCommitment
//import java.time.Instant
//import org.scalacheck.Arbitrary.arbitrary
//import org.scalacheck.{Arbitrary, Gen}
//import scala.concurrent.duration.{FiniteDuration, HOURS}
//import scalus.builtin.ByteString
//import scalus.builtin.Data.toData
//import scalus.cardano.address.{Network, ShelleyAddress}
//import scalus.cardano.ledger.*
//import scalus.cardano.ledger.ArbitraryInstances.given
//import scalus.cardano.ledger.DatumOption.Inline
//import scalus.cardano.ledger.LedgerToPlutusTranslation.getValue
//import scalus.cardano.ledger.TransactionOutput.Babbage
//import scalus.cardano.txbuilder.TransactionBuilder.ensureMinAda
//import scalus.ledger.api.v1
//import scalus.ledger.api.v1.Value.valueOrd
//import scalus.prelude.Ord.<=
//import scalus.prelude.{Option as SOption, Ord}
//import test.*
//import test.Generators.Hydrozoa.*
//import test.Generators.Other
//import test.TestPeer.{Alice, mkWallet}
//
//def genDepositDatum(network: Network = testNetwork): Gen[DepositUtxo.Datum] = {
//    for {
//        address <- genPubkeyAddress(network = network).map(
//          LedgerToPlutusTranslation.getAddress(_).credential
//        )
//        datum <- genByteStringData
//        deadline <- Gen.posNum[BigInt]
//        refundAddress <- genPubkeyAddress(network = network).map(
//          LedgerToPlutusTranslation.getAddress(_)
//        )
//        genData = Gen.frequency(
//          (99, genByteStringData.map(data => SOption.Some(data))),
//          (1, SOption.None)
//        )
//        refundDatum <- genData
//
//    } yield DepositUtxo.Datum(
//      DepositUtxo.Refund.Instructions(
//        address = refundAddress,
//        datum = refundDatum,
//        startTime = QuantizedInstant(
//          testTxBuilderCardanoInfo.slotConfig,
//          java.time.Instant.ofEpochMilli(deadline.toLong)
//        )
//      )
//    )
//}
//
//// FIXME: This way of generating deposit Utxos was fine for earlier iterations of hydrozoa.
//// As we get closer to a real implementation, it needs to be revised to catch more corner cases.
//// In particular, with the introduction of "Virtual Utxos" in the deposit utxo, and with the deposit amount
//// set deterministically from the sum of the virtual utxos + fees necessary for refund, this
//// generator DOES NOT produce actual semantically valid deposit utxos
//def genDepositUtxo(
//    network: Network = testNetwork,
//    params: ProtocolParams = blockfrost544Params,
//    headAddress: Option[ShelleyAddress] = None,
//): Gen[DepositUtxo] =
//    for {
//        txId <- arbitrary[TransactionInput]
//        headAddress_ = headAddress.getOrElse(genScriptAddress(network).sample.get)
//        dd <- genDepositDatum(network)
//
//        // Mock utxo to calculate minAda
//        mockUtxo = Babbage(
//          address = headAddress_,
//          value = Value.zero,
//          datumOption = Some(Inline(dd.toData)),
//          scriptRef = None
//        )
//
//        depositMinAda = ensureMinAda(mockUtxo, params).value.coin
//
//        depositAmount <- arbitrary[Coin].map(_ + depositMinAda).map(Value(_))
//
//        // NOTE: these genesis obligations are completely arbitrary and WILL NOT be coherent with the
//        // deposit amount
//        vos <- Gen.nonEmptyListOf(genGenesisObligation(Alice)).map(NonEmptyList.fromListUnsafe)
//
//        absorptionStart <- Gen
//            .posNum[Long]
//            .map(offsetFromZero =>
//                // Generate some offset to the "zero slot" time.
//                // This is necessary because scalus can't currently support negative numbers as slots
//                Instant.ofEpochMilli(
//                  testTxBuilderCardanoInfo.slotConfig
//                      .slotToTime(testTxBuilderCardanoInfo.slotConfig.zeroSlot)
//                      + offsetFromZero
//                )
//            )
//        // The end is SPECIFIED as the start, plus the deposit absorption duration. If you need to pass in
//        // a non-default tx timing in the future, feel free.
//        absorptionEnd = absorptionStart.quantize(testTxBuilderCardanoInfo.slotConfig)
//            + TxTiming.default(testTxBuilderCardanoInfo.slotConfig).depositAbsorptionDuration
//    } yield DepositUtxo(
//      l1Input = txId,
//      l1OutputAddress = headAddress_,
//      l1OutputDatum = dd,
//      l1OutputValue = depositAmount,
//      virtualOutputs = vos
//    )
//
///** Generate a "standalone" settlement tx. */
//def genSettlementTxSeqBuilder(
//    estimatedFee: Coin = Coin(5_000_000L),
//    params: ProtocolParams = blockfrost544Params,
//    network: Network = testNetwork,
//    // If passed, the kzg commitment will be set to the value.
//    // If not, its randomly generated
//    kzgCommitment: Option[KzgCommitment] = None,
//    fallbackValidityStart: QuantizedInstant = java.time.Instant
//        .ofEpochMilli(java.time.Instant.now().toEpochMilli + 3_600_000)
//        .quantize(testTxBuilderCardanoInfo.slotConfig),
//    blockCreatedOn: QuantizedInstant =
//        java.time.Instant.now().quantize(testTxBuilderCardanoInfo.slotConfig),
//    txTiming: TxTiming = TxTiming.default(testTxBuilderCardanoInfo.slotConfig)
//): Gen[(SettlementTxSeq.Build, SettlementTxSeq.Build.Args, NonEmptyList[TestPeer])] = {
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
//        seedUtxo <- arbitrary[TransactionInput]
//        tokenNames = HeadTokenNames(seedUtxo)
//        mw <- genFakeMultisigWitnessUtxo(
//          hms,
//          network,
//          Some(tokenNames.multisigRegimeTokenName)
//        )
//        majorVersion <- Gen.posNum[Int]
//
//        genDeposit = genDepositUtxo(
//          network = network,
//          params = params,
//          headAddress = Some(hms.mkAddress(network))
//        )
//        deposits <- genHelper(genDeposit)
//
//        payouts <- genHelper(genPayoutObligation(network))
//        payoutAda = payouts
//            .map(_.utxo.value.coin)
//            .fold(Coin.zero)(_ + _)
//        utxo <- genTreasuryUtxo(
//          headAddress = Some(hms.mkAddress(network)),
//          network = network,
//          coin = Some(payoutAda + Coin(1_000_000_000L))
//        )
//
//        kzg: KzgCommitment <- kzgCommitment match {
//            case None =>
//                Gen.listOfN(48, Arbitrary.arbitrary[Byte])
//                    .map(list => ByteString.fromArray(list.toArray))
//            case Some(kzg) => Gen.const(kzg)
//        }
//
//        config = SettlementTxSeq.Config(
//          headMultisigScript = hms,
//          multisigRegimeUtxo = mw,
//          tokenNames = tokenNames,
//          // TODO: verify
//          votingDuration = FiniteDuration(24, HOURS).quantize(testTxBuilderCardanoInfo.slotConfig),
//          txTiming = txTiming,
//          // TODO: verify
//          tallyFeeAllowance = Coin.ada(2),
//          cardanoInfo = testTxBuilderCardanoInfo
//        )
//    } yield (
//      SettlementTxSeq.Build(config),
//      SettlementTxSeq.Build.Args(
//        kzgCommitment = kzg,
//        majorVersionProduced = BlockVersion.Major(majorVersion),
//        depositsToSpend = deposits,
//        payoutObligationsRemaining = payouts,
//        treasuryToSpend = utxo,
//        competingFallbackValidityStart = fallbackValidityStart,
//        blockCreatedOn = blockCreatedOn
//      ),
//      peers
//    )
//}
//
///** Generates the settlement seq builder for the next settlement tx that should correctly spend the
//  * [[treasuryToSpend]].
//  * @param treasuryToSpend
//  *   the treasury to be spent in the settlement sequence
//  * @param majorVersion
//  *   the version of the next block
//  */
//def genNextSettlementTxSeqBuilder(
//    config: SettlementTxSeq.Config,
//    treasuryToSpend: MultisigTreasuryUtxo,
//    fallbackValidityStart: QuantizedInstant,
//    blockCreatedOn: QuantizedInstant,
//    majorVersion: Int,
//    headNativeScript: HeadMultisigScript,
//    txTiming: TxTiming,
//    estimatedFee: Coin = Coin(5_000_000L),
//    params: ProtocolParams = blockfrost544Params,
//    network: Network = testNetwork,
//    // If passed, the kzg commitment will be set to the value.
//    // If not, its randomly generated
//    kzgCommitment: Option[KzgCommitment] = None
//): Gen[(SettlementTxSeq.Build, SettlementTxSeq.Build.Args)] = {
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
//    val genDeposit = genDepositUtxo(
//      network = network,
//      params = params,
//      headAddress = Some(headNativeScript.mkAddress(network))
//    )
//
//    given Ord[v1.Value] = valueOrd
//
//    for {
//        deposits <- genHelper(genDeposit)
//        payouts <- genHelper(genPayoutObligation(network))
//        prefixes = (payouts.length to 0 by -1).map(payouts.take)
//        infimum = prefixes
//            .find(prefix =>
//                getValue(
//                  prefix
//                      .map(_.utxo.value)
//                      .fold(Value.zero)(_ + _)
//                ) <= getValue(treasuryToSpend.value)
//            )
//            // Since we have empty prefix that always satisfies the condition this is safe
//            .get
//        kzg: KzgCommitment <- kzgCommitment match {
//            case None =>
//                Gen.listOfN(48, Arbitrary.arbitrary[Byte])
//                    .map(list => ByteString.fromArray(list.toArray))
//            case Some(kzg) => Gen.const(kzg)
//        }
//    } yield (
//      SettlementTxSeq.Build(config),
//      SettlementTxSeq.Build.Args(
//        kzgCommitment = kzg,
//        majorVersionProduced = BlockVersion.Major(majorVersion),
//        depositsToSpend = deposits,
//        payoutObligationsRemaining = infimum,
//        treasuryToSpend = treasuryToSpend,
//        competingFallbackValidityStart = fallbackValidityStart,
//        blockCreatedOn = blockCreatedOn
//      ),
//    )
//}
