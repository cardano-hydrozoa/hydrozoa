package hydrozoa.config

import org.scalacheck.Properties

//import cats.data.NonEmptyList
//import cats.effect.IO
//import cats.effect.unsafe.implicits.global
//import cats.syntax.all.{catsSyntaxEither, toFlatMapOps}
//import hydrozoa.config.HeadConfig.OwnPeer
//import hydrozoa.config.head.multisig.timing.TxTiming
//import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant.realTimeQuantizedInstant
//import hydrozoa.lib.cardano.scalus.QuantizedTime.{QuantizedFiniteDuration, quantize}
//import hydrozoa.multisig.consensus.peer.HeadPeerId
//import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
//import hydrozoa.multisig.ledger.dapp.tx.InitializationTx.SpentUtxos
//import hydrozoa.multisig.ledger.dapp.tx.minInitTreasuryAda
//import hydrozoa.multisig.ledger.dapp.txseq.InitializationTxSeq
//import hydrozoa.rulebased.ledger.dapp.tx.CommonGenerators.genShelleyAddress
//import hydrozoa.rulebased.ledger.dapp.tx.genEquityShares
//import hydrozoa.{Address, L1, maxNonPlutusTxFee}
//import org.scalacheck.{Gen, Properties, PropertyM}
//import scala.concurrent.duration.{DurationInt, FiniteDuration, HOURS}
//import scalus.builtin.ByteString
//import scalus.cardano.address.ShelleyAddress
//import scalus.cardano.address.ShelleyPaymentPart.Key
//import scalus.cardano.ledger.{AddrKeyHash, Coin, SlotConfig, Utxo, Value}
//import scalus.cardano.txbuilder.TransactionBuilder.ResolvedUtxos
//import spire.math.UByte
//import test.Generators.Hydrozoa.genAdaOnlyPubKeyUtxo
//import test.{TestPeer, genTestPeers, minPubkeyAda, sumUtxoValues, testTxBuilderCardanoInfo}

object HeadConfigTest extends Properties("HeadConfig Test") {
//    val _ = property("sanity test for genRawConfig") = PropertyM.monadicIO(
//      for {
//          _ <- genRawConfig
//      } yield true
//    )
//
//    val _ = property("sanity test for genHeadConfig") = PropertyM.monadicIO(
//      for {
//          _ <- genHeadConfig
//      } yield true
//    )
}

///** Generate a RawConfig.
//  *
//  * This is intended to be THE general-purpose generator for tests that test the behavior of
//  * components that run on a single node.
//  *
//  * It is recommended that you use this as your base and project out to individual components via
//  * parsing into a HeadConfig. This will give us consistency in labeling and coverage, as well as
//  * ensure internal coherence of the invariants on HeadConfig.
//  *
//  * It also simulates an environment closer to production, where everything will be parsed from a
//  * RawConfig anyway.
//  *
//  * Note that this is generating, building, and parsing an InitializationTxSeq. Thus, you should
//  * most likely re-use this value in the majority of your tests.
//  */
//val genRawConfig: PropertyM[IO, RawConfig] =
//
//    // TODO: Should be gone after refactoring
//    val mainnetSlotConfig: SlotConfig = testTxBuilderCardanoInfo.slotConfig
//
//    for {
//        // NOTE: I'm going to do everything inline here for now. It will be better to break it up in the future, and this
//        // may replace some of the other generators with ones that have better labeling (since they use PropertyM).
//
//        ////////////////////////////////////////////////////
//        // Set up peers
//        ////////////////////////////////////////////////////
//
//        // This doesn't necessarily need to be impure
//        startTime <- PropertyM.run(realTimeQuantizedInstant(mainnetSlotConfig))
//
//        // We need a wallet for handling withdrawals. We do this by just generating 3 peers and using
//        // the tail as the "actual" peers.
//        withdrawalPlusPeers <- PropertyM.pick[IO, NonEmptyList[TestPeer]](
//          genTestPeers(minPeers = 3)
//              .label("Test peers (first is withdrawal wallet)")
//        )
//
//        peers = NonEmptyList.fromListUnsafe(withdrawalPlusPeers.tail)
//
//        equityShares <- PropertyM.pick[IO, EquityShares](
//          genEquityShares(peers).label("equity shares")
//        )
//
//        // TODO: Better coverage might include:
//        // - Duplicates in this list
//        // - Overlap between this list and the peer addresses
//        payoutAddresses <- PropertyM.pick[IO, List[ShelleyAddress]](
//          Gen.listOfN(
//            peers.size,
//            genShelleyAddress().label("Payout addresses for contingency/equity")
//          )
//        )
//
//        ownPeerIndex <- PropertyM.pick[IO, Int](Gen.choose(0, peers.size - 1).label("Own Peer Id"))
//
//        ownPeer = (
//          OwnPeer(HeadPeerId(ownPeerIndex, peers.size), peers.toList(ownPeerIndex).wallet),
//          Address[L1](payoutAddresses(ownPeerIndex)),
//          equityShares.peerShares(UByte(ownPeerIndex)).equityShare
//        )
//
//        otherPeers = peers.toList.zipWithIndex
//            .foldRight(List.empty[PeerSection])((testPeerWithIndex, l) =>
//                if testPeerWithIndex._2 == ownPeerIndex
//                then l
//                else
//                    l.prepended(
//                      PeerSection(
//                        verificationKeyBytes =
//                            testPeerWithIndex._1.wallet.exportVerificationKeyBytes,
//                        payoutAddress = Address[L1](payoutAddresses(testPeerWithIndex._2)),
//                        equityShare =
//                            equityShares.peerShares(UByte(testPeerWithIndex._2)).equityShare
//                      )
//                    )
//            )
//
//        /////////////////////////////////////////////
//        // Set up initialization and fallback
//        /////////////////////////////////////////////
//
//        // NOTE: this is primarily yoinked from JointLedgerTestHelpers.defaultInitializer
//
//        // We make sure that the seed utxo has at least enough for the treasury and multisig witness UTxO, plus
//        // a max non-plutus fee
//        seedUtxo <- PropertyM.pick[IO, Utxo](
//          genAdaOnlyPubKeyUtxo(
//            peers.head,
//            minimumCoin = minInitTreasuryAda
//                + Coin(maxNonPlutusTxFee(testTxBuilderCardanoInfo.protocolParams).value * 2)
//          ).map(x => Utxo(x._1, x._2)).label("Initialization: seed utxo")
//        )
//
//        otherSpentUtxos <- PropertyM.pick[IO, List[Utxo]](
//          Gen
//              .listOf(genAdaOnlyPubKeyUtxo(peers.head))
//              .map(_.map(x => Utxo(x._1, x._2)))
//              .label("Initialization: other spent utxos")
//        )
//
//        spentUtxos = NonEmptyList(seedUtxo, otherSpentUtxos)
//
//        // Initial treasury must be at least enough for the minAda of the treasury, and no more than the
//        // sum of the seed+funding utxos, while leaving enough left for the estimated fee and the minAda of the change
//        // output
//        initialTreasuryCoin <- PropertyM.pick[IO, Coin](
//          Gen
//              .choose(
//                minInitTreasuryAda.value,
//                sumUtxoValues(spentUtxos.toList).coin.value
//                    - maxNonPlutusTxFee(testTxBuilderCardanoInfo.protocolParams).value
//                    - minPubkeyAda().value
//              )
//              .map(Coin(_))
//              .label("Initialization: initial treasury coin")
//        )
//
//        initialTreasury = Value(initialTreasuryCoin)
//
//        txTiming = TxTiming.default(mainnetSlotConfig)
//
//        initTxConfig = InitializationTxSeq.Config(
//          tallyFeeAllowance = Coin.ada(2),
//          votingDuration = FiniteDuration(24, HOURS).quantize(mainnetSlotConfig),
//          cardanoInfo = testTxBuilderCardanoInfo,
//          peerKeys = peers.map(_.wallet.exportVerificationKeyBytes),
//          startTime = startTime,
//          txTiming = txTiming
//        )
//
//        initTxArgs =
//            InitializationTxSeq.Builder.Args(
//              spentUtxos = SpentUtxos(seedUtxo, otherSpentUtxos),
//              initialTreasury = initialTreasury,
//              initializationTxChangePP =
//                  Key(AddrKeyHash.fromByteString(ByteString.fill(28, 1.toByte))),
//            )
//
//        headMultisigScript = HeadMultisigScript(peers.map(_.wallet.exportVerificationKeyBytes))
//
//        initTxSeq <- PropertyM.run(
//          InitializationTxSeq.Builder.build(initTxArgs, initTxConfig).liftTo[IO]
//        )
//
//        rawConfig: RawConfig = RawConfig(
//          ownPeer = ownPeer,
//          // CHECK ME: Is this reverse the order of peers so that our generated "own peer id" is wrong?
//          // I can never remember the order of folds, but I'm pretty sure I need foldRight here
//          otherPeers = otherPeers,
//          receiveTimeout = 10.seconds,
//          initializationTxBytes = initTxSeq.initializationTx.tx.toCbor,
//          initialFallbackTxBytes = initTxSeq.fallbackTx.tx.toCbor,
//          network = Left(StandardCardanoNetwork.Mainnet),
//          tallyFeeAllowance = Coin.ada(2),
//          votingDuration = QuantizedFiniteDuration(
//            finiteDuration = 24.hours,
//            slotConfig = mainnetSlotConfig
//          ),
//          txTiming = txTiming,
//          startTime = startTime,
//          resolvedUtxosForInitialization =
//              ResolvedUtxos(Map.from(spentUtxos.toList.map(_.toTuple))),
//          withdrawalFeeWallet = withdrawalPlusPeers.head.wallet,
//          pollingPeriod = 5.seconds
//        )
//    } yield rawConfig
//
///** Can be used as the initializer (or a component thereof) for a TestM[HeadConfig, A]
//  */
//val genHeadConfig: PropertyM[IO, HeadConfig] =
//    for {
//        rawConfig <- genRawConfig
//        headConfig <- PropertyM.run[IO, HeadConfig](HeadConfig.parse(rawConfig).liftTo[IO])
//    } yield headConfig
