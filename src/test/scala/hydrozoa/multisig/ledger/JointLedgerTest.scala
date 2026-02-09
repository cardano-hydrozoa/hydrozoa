package hydrozoa.multisig.ledger

//import cats.*
//import cats.data.*
//import cats.effect.*
//import cats.effect.unsafe.implicits.*
//import cats.syntax.all.*
//import com.suprnation.actor.Actor.{Actor, Receive}
//import com.suprnation.actor.ActorRef.ActorRef
//import com.suprnation.actor.{ActorSystem, test as _}
//import hydrozoa.config.EquityShares
//import hydrozoa.lib.cardano.scalus.QuantizedTime.*
//import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant.realTimeQuantizedInstant
//import hydrozoa.maxNonPlutusTxFee
//import hydrozoa.multisig.consensus.ConsensusActor
//import hydrozoa.multisig.consensus.ConsensusActor.Request
//import hydrozoa.multisig.consensus.peer.PeerId
//import hydrozoa.multisig.ledger.JointLedger.Requests.{CompleteBlockFinal, CompleteBlockRegular, StartBlock}
//import hydrozoa.multisig.ledger.JointLedger.{Done, Producing}
//import hydrozoa.multisig.ledger.JointLedgerTestHelpers.*
//import hydrozoa.multisig.ledger.JointLedgerTestHelpers.Requests.{completeBlockRegular, getState, startBlockNow}
//import hydrozoa.multisig.ledger.JointLedgerTestHelpers.Scenarios.{deposit, unsafeGetDone, unsafeGetProducing}
//import hydrozoa.multisig.ledger.block.{Block, BlockBrief, BlockEffects, BlockHeader, BlockNumber, BlockVersion}
//import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
//import hydrozoa.multisig.ledger.dapp.token.CIP67.TokenNames
//import hydrozoa.multisig.ledger.dapp.tx.InitializationTx.SpentUtxos
//import hydrozoa.multisig.ledger.dapp.tx.{TxTiming, minInitTreasuryAda}
//import hydrozoa.multisig.ledger.dapp.txseq.{DepositRefundTxSeq, InitializationTxSeq}
//import hydrozoa.multisig.ledger.dapp.utxo.{DepositUtxo, MultisigRegimeUtxo}
//import hydrozoa.multisig.ledger.event.LedgerEvent.RegisterDeposit
//import hydrozoa.multisig.ledger.event.LedgerEventId
//import hydrozoa.multisig.ledger.event.LedgerEventId.ValidityFlag.{Invalid, Valid}
//import hydrozoa.multisig.ledger.virtual.L2EventGenesis
//import hydrozoa.multisig.ledger.virtual.commitment.KzgCommitment
//import hydrozoa.rulebased.ledger.dapp.tx.genEquityShares
//import io.bullet.borer.Cbor
//import java.util.concurrent.TimeUnit
//import org.scalacheck.*
//import org.scalacheck.Prop.propBoolean
//import org.scalacheck.PropertyM.monadForPropM
//import scala.collection.immutable.Queue
//import scala.concurrent.duration.{DurationInt, FiniteDuration, HOURS}
//import scalus.builtin.ByteString
//import scalus.cardano.address.ShelleyPaymentPart.Key
//import scalus.cardano.ledger.{AddrKeyHash, Block as _, BlockHeader as _, Coin, Utxo, *}
//import scalus.prelude.Option as SOption
//import test.*
//import test.Generators.Hydrozoa.*
//import test.Generators.Other.genCoinDistributionWithMinAdaUtxo
//import test.TestM.*
//
///** This object contains component-specific helpers to utilize the TestM type.
//  *
//  * It defines the following bits:
//  *
//  *   - A [[JLTest]] type alias over [[TestM]], to cut down on type signature noise
//  *   - A [[TestR]] environment, specific to the joint ledger tests
//  *   - A [[defaultInitializer]] that initializers the TestR environment under which the joint
//  *     ledger tests run
//  *   - A [[Requests]] object that "thinly" lifts [[IO]] actor requests into [[JLTest]], which
//  *     simplifies writing monadic code. "Thinly" here means that the defined functions don't
//  *     _morally_ add additional effects.
//  *     - One exception to this may be testing pre-conditions or post-conditions; for example, every
//  *       non-exception-throwing call to `completeBlockRegular` should result transition of a
//  *       [[JointLedger.State]] from [[Producing]] to [[Done]], but this can only be accomplished by
//  *       sending two additional messages to the JointLedger actor in order to observe the state
//  *       before and after. If your test is sensitive to the EXACT messages that are sent to the
//  *       actor's mailbox, then you may need to make some modifications.
//  *   - A [[Scenarios]] object that includes pre-defined, composable test snippets, making full use
//  *     of the embedded PropertyM and IO of the JLTest.
//  */
//object JointLedgerTestHelpers {
//
//    type JLTest[A] = TestM[TestR, A]
//
//    /** The "environment" that is contained in the ReaderT of the JLTest
//      */
//    case class TestR(
//        peers: NonEmptyList[TestPeer],
//        actorSystem: ActorSystem[IO],
//        initTx: InitializationTxSeq, // Move to HeadConfig
//        jointLedger: ActorRef[IO, JointLedger.Requests.Request],
//        txTiming: TxTiming, // Move to HeadConfig
//        tokenNames: TokenNames,
//        multisigRegimeUtxo: MultisigRegimeUtxo
//    )
//
//    // TODO: Right now, this generates everything. In the future, we can provide arguments like
//    // `peers :: Option[NonEmptyList[TestPeer]]` such that we set the peers exactly to the option,
//    // or generate otherwise. This goes along with the comment on [[run]] for passing initializers directly to run
//    val defaultInitializer: PropertyM[IO, TestR] = {
//        for {
//
//            peers <- PropertyM.pick[IO, NonEmptyList[TestPeer]](genTestPeers().label("Test Peers"))
//
//            // We make sure that the seed utxo has at least enough for the treasury and multisig witness UTxO, plus
//            // a max non-plutus fee
//            seedUtxo <- PropertyM.pick[IO, Utxo](
//              genAdaOnlyPubKeyUtxo(
//                peers.head,
//                minimumCoin = minInitTreasuryAda
//                    + Coin(maxNonPlutusTxFee(testProtocolParams).value * 2)
//              ).map(x => Utxo(x._1, x._2)).label("Initialization: seed utxo")
//            )
//
//            otherSpentUtxos <- PropertyM.pick[IO, List[Utxo]](
//              Gen
//                  .listOf(genAdaOnlyPubKeyUtxo(peers.head))
//                  .map(_.map(x => Utxo(x._1, x._2)))
//                  .label("Initialization: other spent utxos")
//            )
//
//            spentUtxos = NonEmptyList(seedUtxo, otherSpentUtxos)
//
//            // Initial treasury must be at least enough for the minAda of the treasury, and no more than the
//            // sum of the seed+funding utxos, while leaving enough left for the estimated fee and the minAda of the change
//            // output
//            initialTreasuryCoin <- PropertyM.pick[IO, Coin](
//              Gen
//                  .choose(
//                    minInitTreasuryAda.value,
//                    sumUtxoValues(spentUtxos.toList).coin.value
//                        - maxNonPlutusTxFee(testTxBuilderCardanoInfo.protocolParams).value
//                        - minPubkeyAda().value
//                  )
//                  .map(Coin(_))
//                  .label("Initialization: initial treasury coin")
//            )
//
//            initialTreasury = Value(initialTreasuryCoin)
//
//            txTiming = TxTiming.default(testTxBuilderCardanoInfo.slotConfig)
//
//            startTime <- PropertyM.run(
//              realTimeQuantizedInstant(testTxBuilderCardanoInfo.slotConfig)
//            )
//
//            initTxConfig = InitializationTxSeq.Config(
//              tallyFeeAllowance = Coin.ada(2),
//              votingDuration =
//                  FiniteDuration(24, HOURS).quantize(testTxBuilderCardanoInfo.slotConfig),
//              cardanoInfo = testTxBuilderCardanoInfo,
//              peerKeys = peers.map(_.wallet.exportVerificationKeyBytes),
//              startTime = startTime,
//              txTiming = txTiming
//            )
//
//            initTxArgs =
//                InitializationTxSeq.Builder.Args(
//                  spentUtxos = SpentUtxos(seedUtxo, otherSpentUtxos),
//                  initialTreasury = initialTreasury,
//                  initializationTxChangePP =
//                      Key(AddrKeyHash.fromByteString(ByteString.fill(28, 1.toByte))),
//                )
//
//            headMultisigScript = HeadMultisigScript(peers.map(_.wallet.exportVerificationKeyBytes))
//
//            system <- PropertyM.run(ActorSystem[IO]("DappLedger").allocated.map(_._1))
//            initializationTxSeq <- PropertyM.run(
//              InitializationTxSeq.Builder.build(initTxArgs, initTxConfig).liftTo[IO]
//            )
//
//            equityShares <- PropertyM.pick[IO, EquityShares](
//              genEquityShares(peers).label("Equity shares")
//            )
//
//            multisigRegimeUtxo = initializationTxSeq.initializationTx.multisigRegimeUtxo
//
//            tokenNames = initializationTxSeq.initializationTx.tokenNames
//
//            consensusActorStub <- PropertyM.run(
//              system.actorOf(new Actor[IO, ConsensusActor.Request] {
//                  override def receive: Receive[IO, ConsensusActor.Request] = _ => IO.unit
//              })
//            )
//
//            initialBlock = Block.MultiSigned.Initial(
//              blockBrief = BlockBrief.Initial(header =
//                  BlockHeader
//                      .Initial(startTime = startTime, kzgCommitment = KzgCommitment.empty)
//              ),
//              effects = BlockEffects.MultiSigned.Initial(
//                initializationTx = initializationTxSeq.initializationTx,
//                fallbackTx = initializationTxSeq.fallbackTx
//              )
//            )
//
//            jointLedger <- PropertyM.run(
//              system.actorOf(
//                JointLedger(
//                  JointLedger.Config(
//                    initialBlock = initialBlock,
//                    peerId = PeerId(peers.head.ordinal, peers.size),
//                    wallet = peers.head.wallet,
//                    tallyFeeAllowance = Coin.ada(2),
//                    tokenNames = tokenNames,
//                    headMultisigScript = headMultisigScript,
//                    cardanoInfo = testTxBuilderCardanoInfo,
//                    initialBlockTime = startTime,
//                    initialBlockKzg = KzgCommitment.empty,
//                    equityShares = equityShares,
//                    multisigRegimeUtxo = multisigRegimeUtxo,
//                    votingDuration =
//                        FiniteDuration(24, HOURS).quantize(testTxBuilderCardanoInfo.slotConfig),
//                    initialTreasury = initializationTxSeq.initializationTx.treasuryProduced,
//                    txTiming = txTiming,
//                    initialFallbackValidityStart =
//                        startTime + txTiming.minSettlementDuration + txTiming.inactivityMarginDuration + txTiming.silenceDuration
//                  ),
//                  JointLedger.Connections(
//                    consensusActor = consensusActorStub,
//                    peerLiaisons = List()
//                  )
//                )
//              )
//            )
//
//        } yield TestR(
//          peers,
//          system,
//          initializationTxSeq,
//          jointLedger,
//          txTiming,
//          tokenNames,
//          multisigRegimeUtxo
//        )
//    }
//
//    /** Helper utilities to send actor Requests to the JointLedger
//      */
//    object Requests {
//
//        val getState: JLTest[JointLedger.State] =
//            for {
//                env <- ask
//                state <- lift(env.jointLedger ?: JointLedger.Requests.GetState)
//            } yield state
//
//        def registerDeposit(req: RegisterDeposit): JLTest[Unit] = {
//            for {
//                jl <- asks[TestR, ActorRef[IO, JointLedger.Requests.Request]](_.jointLedger)
//                _ <- lift(jl ? req)
//            } yield ()
//        }
//
//        def startBlock(req: StartBlock): JLTest[Unit] =
//            ask.flatMap(env => lift(env.jointLedger ! req))
//
//        def startBlock(
//            blockNum: BlockNumber,
//            blockCreationTime: QuantizedInstant,
//        ): JLTest[Unit] =
//            startBlock(StartBlock(blockNum, blockCreationTime))
//
//        /** Start the block at the current real time */
//        def startBlockNow(blockNum: BlockNumber): JLTest[QuantizedInstant] =
//            for {
//                startTime <- lift(realTimeQuantizedInstant(testTxBuilderCardanoInfo.slotConfig))
//                _ <- startBlock(blockNum, startTime)
//            } yield startTime
//
//        /** WAARNING: This method performs pre-and-post condition checks on the joint ledger. This
//          * means two things:
//          *   - This will send three messages to the JointLedger -- two to check the state before
//          *     and after, and one to actually send the CompleteBlockRegular request
//          *   - There is a race condition if this function is used in a context where multiple
//          *     messages are being sent to the JointLedger from different sources
//          *
//          * I (Peter) am trying this out for now (2026-01-08), but I suspect I'll want a better way
//          * to do this in the future -- feel free to remove these checks if they're annoying.
//          * Perhaps hacking something into to cats-actors so that I can send multiple messages that
//          * I know will be processed as a batch is a way forward?
//          */
//        def completeBlockRegular(req: CompleteBlockRegular): JLTest[Unit] =
//            for {
//                jl <- asks[TestR, ActorRef[IO, JointLedger.Requests.Request]](_.jointLedger)
//                beforeState <- lift(jl ?: JointLedger.Requests.GetState)
//                _ <- lift(jl ! req)
//                afterState <- lift(jl ?: JointLedger.Requests.GetState)
//                _ <- assertWith(
//                  condition = beforeState.isInstanceOf[JointLedger.Producing],
//                  msg = "A CompleteBlockRegular request was sent to the JointLedger and it succeeded, but" +
//                      " the JointLedger wasn't in the Producing state before the request was sent"
//                )
//                _ <- assertWith(
//                  condition = afterState.isInstanceOf[JointLedger.Done],
//                  msg = "A CompleteBlockRegular request was sent to the JointLedger and it succeeded, but" +
//                      " the JointLedger didn't transition to the Done state after the request was processed"
//                )
//            } yield ()
//
//        def completeBlockRegular(
//            referenceBlock: Option[BlockBrief.Intermediate],
//            pollResults: Set[TransactionInput]
//        ): JLTest[Unit] =
//            completeBlockRegular(
//              CompleteBlockRegular(referenceBlock, pollResults: Set[TransactionInput], false)
//            )
//
//        def completeBlockFinal(req: CompleteBlockFinal): JLTest[Unit] =
//            for {
//                jl <- asks[TestR, ActorRef[IO, JointLedger.Requests.Request]](_.jointLedger)
//                _ <- lift(jl ! req)
//            } yield ()
//
//        def completeBlockFinal(
//            referenceBlock: Option[BlockBrief.Final],
//        ): JLTest[Unit] =
//            completeBlockFinal(
//              CompleteBlockFinal(referenceBlockBrief = referenceBlock)
//            )
//
//    }
//
//    /** Helper utilties to execute particular scenarios, such as "generating a random deposit and
//      * sending it to the JointLedger"
//      */
//    object Scenarios {
//
//        def unsafeGetProducing: JLTest[Producing] =
//            for {
//                state <- getState
//                p <- state match {
//                    case _: Done => throw RuntimeException("Expected a Producing State, got Done")
//                    case p: Producing => TestM.pure(p)
//                }
//            } yield p
//
//        def unsafeGetDone: JLTest[Done] =
//            for {
//                state <- getState
//                d <- state match {
//                    case d: Done => TestM.pure(d)
//                    case _: Producing =>
//                        throw RuntimeException("Expected a Done State, got Producing")
//                }
//            } yield d
//
//        /** Generate a random (sensible) deposit from the first peer and send it to the joint ledger
//          */
//        def deposit(
//            validityEnd: QuantizedInstant,
//            eventId: LedgerEventId,
//            blockStartTime: QuantizedInstant
//        ): JLTest[(DepositRefundTxSeq, RegisterDeposit)] = {
//            import Requests.*
//            for {
//                env <- ask[TestR]
//                peer = env.peers.head
//                virtualOutputs <-
//                    pick(
//                      Gen.nonEmptyListOf(genGenesisObligation(peer, minimumCoin = Coin.ada(5)))
//                          .map(NonEmptyList.fromListUnsafe)
//                          .label(s"Virtual Outputs for deposit $eventId")
//                    )
//
//                virtualOutputsBytes =
//                    Cbor
//                        .encode(
//                          virtualOutputs.toList.map(_.toBabbage.asInstanceOf[TransactionOutput])
//                        )
//                        .toByteArray
//
//                virtualOutputsValue = Value.combine(
//                  virtualOutputs.map(vo => Value(vo.l2OutputValue)).toList
//                )
//
//                utxosFunding <- pick((for {
//                    utxosWith0Coin <- Gen
//                        .nonEmptyListOf(
//                          genAdaOnlyPubKeyUtxo(peer, minimumCoin = Coin.ada(3))
//                        )
//                    utxoDist <- genCoinDistributionWithMinAdaUtxo(
//                      virtualOutputsValue.coin,
//                      NonEmptyList.fromListUnsafe(utxosWith0Coin),
//                      testProtocolParams
//                    )
//                } yield utxoDist).label("Funding Utxos"))
//
//                utxosFundingValue = Value.combine(utxosFunding.toList.map(_._2.value))
//
//                headMultisigScript = HeadMultisigScript(
//                  env.peers.map(_.wallet.exportVerificationKeyBytes)
//                )
//
//                depositRefundSeqConfig = DepositRefundTxSeq.Config(
//                  txTiming = env.txTiming,
//                  cardanoInfo = testTxBuilderCardanoInfo,
//                  headMultisigScript = headMultisigScript,
//                  multisigRegimeUtxo = env.multisigRegimeUtxo
//                )
//
//                depositRefundSeqBuilder = DepositRefundTxSeq.Builder(
//                  config = depositRefundSeqConfig,
//                  refundInstructions = DepositUtxo.Refund.Instructions(
//                    LedgerToPlutusTranslation.getAddress(peer.address(testNetwork)),
//                    SOption.None,
//                    validityEnd
//                        + env.txTiming.depositMaturityDuration
//                        + env.txTiming.depositAbsorptionDuration
//                        + env.txTiming.silenceDuration
//                  ),
//                  donationToTreasury = Coin.zero,
//                  refundValue = virtualOutputsValue,
//                  virtualOutputs = virtualOutputs,
//                  changeAddress = peer.address(testNetwork),
//                  utxosFunding = utxosFunding,
//                )
//
//                depositRefundTxSeq <- lift(depositRefundSeqBuilder.build.liftTo[IO])
//
//                // refund(i).validity_start = deposit(i).absorption_end + silence_period
//                // refund(i).validity_end = ∅
//                _ <- assertWith(
//                  msg = "refund start validity is incorrect",
//                  condition = {
//                      depositRefundTxSeq.refundTx.tx.body.value.validityStartSlot.isDefined
//                      && Slot(depositRefundTxSeq.refundTx.tx.body.value.validityStartSlot.get)
//                          .toQuantizedInstant(testTxBuilderCardanoInfo.slotConfig)
//                          ==
//                          depositRefundTxSeq.depositTx.validityEnd
//                          + env.txTiming.depositMaturityDuration
//                          + env.txTiming.depositAbsorptionDuration
//                          + env.txTiming.silenceDuration
//                  }
//                )
//
//                _ <- assertWith(
//                  msg = "refund end validity is incorrect",
//                  condition = depositRefundTxSeq.refundTx.tx.body.value.ttl.isEmpty
//                )
//
//                req =
//                    RegisterDeposit(
//                      depositTxBytes = peer.signTx(depositRefundTxSeq.depositTx.tx).toCbor,
//                      refundTxBytes = peer.signTx(depositRefundTxSeq.refundTx.tx).toCbor,
//                      donationToTreasury = Coin.zero,
//                      virtualOutputsBytes = virtualOutputsBytes,
//                      eventId = eventId,
//                      txTiming = env.txTiming,
//                      blockStartTime = blockStartTime
//                    )
//
//                _ <- registerDeposit(req)
//            } yield (depositRefundTxSeq, req)
//        }
//    }
//
//}
//
//object JointLedgerTest extends Properties("Joint Ledger Test") {
//
//    import TestM.*
//
//    override def overrideParameters(p: Test.Parameters): Test.Parameters = {
//        p
//            .withMinSuccessfulTests(100)
//    }
//
//    val _ = property("Joint Ledger Happy Path") =
//        import Requests.*
//        import Scenarios.*
//        run(
//          initializer = defaultInitializer,
//          testM = for {
//              env <- ask[TestR]
//
//              // Put the joint ledger in producing mode
//              startTime <- startBlockNow(BlockNumber.zero.increment)
//
//              // Generate a deposit and observe that it appears in the dapp ledger correctly
//              firstDepositValidityEnd = startTime + 10.minutes
//              seqAndReq <- deposit(
//                validityEnd = firstDepositValidityEnd,
//                eventId = LedgerEventId(0, 1),
//                blockStartTime = startTime
//              )
//              (depositRefundTxSeq, depositReq) = seqAndReq
//
//              _ <- for {
//                  jlState <- getState
//                  dlState = jlState.dappLedgerState
//
//                  _ <- assertWith(
//                    msg =
//                        s"We should have 1 deposit in the state. We have ${dlState.deposits.length}",
//                    condition = dlState.deposits.length == 1
//                  )
//                  _ <- assertWith(
//                    msg = "Correct deposit(s) in state",
//                    condition =
//                        dlState.deposits.head._2 == depositRefundTxSeq.depositTx.depositProduced
//                  )
//                  _ <- assertWith(
//                    msg = "Correct treasury in state",
//                    condition = dlState.treasury == env.initTx.initializationTx.treasuryProduced
//                  )
//              } yield ()
//
//              // Complete a block, but assume the deposit didn't show up in the poll results
//              _ <- completeBlockRegular(None, Set.empty)
//              _ <-
//                  for {
//                      jointLedgerState <- getState
//                      _ <- assertWith(
//                        msg = "Block with no deposits/withdrawal should be Minor",
//                        condition = jointLedgerState match {
//                            case JointLedger.Done(block: Block.Unsigned.Minor, _, _, _) => true
//                            case _                                                      => false
//                        }
//                      )
//                      minorBlock: Block.Unsigned.Minor =
//                          jointLedgerState
//                              .asInstanceOf[JointLedger.Done]
//                              .producedBlock
//                              .asInstanceOf[Block.Unsigned.Minor]
//                      _ <- assertWith(
//                        msg = "Block's deposit absorbed and deposits refunded should both be empty",
//                        condition = minorBlock.body.depositsRefunded.isEmpty
//                            && minorBlock.body.depositsAbsorbed.isEmpty
//                      )
//                  } yield ()
//
//              // Complete another block, assume the deposit shows up in the poll results -- but its not mature yet
//              _ <- startBlockNow(BlockNumber.zero.increment.increment)
//              _ <- completeBlockRegular(
//                None,
//                Set(depositRefundTxSeq.depositTx.depositProduced.toUtxo.input)
//              )
//              _ <- for {
//                  jointLedgerState <- getState
//                  _ <- assertWith(
//                    msg = "Finished block should be minor because no deposits were absorbed",
//                    condition = jointLedgerState match {
//                        case JointLedger.Done(block: Block.Unsigned.Minor, _, _, _) => true
//                        case _                                                      => false
//                    }
//                  )
//              } yield ()
//
//              // Complete another block, including the deposit in the state.
//              _ <- startBlock(
//                BlockNumber.zero.increment.increment.increment,
//                firstDepositValidityEnd + env.txTiming.depositMaturityDuration
//              )
//              _ <- completeBlockRegular(
//                None,
//                Set(depositRefundTxSeq.depositTx.depositProduced.toUtxo.input)
//              )
//
//              _ <- for {
//                  jlState <- getState
//                  majorBlock <- jlState match {
//                      case JointLedger.Done(block: Block.Unsigned.Major, _, _, _) => pure(block)
//                      case _ => fail("FAIL: finished block should be major")
//                  }
//
//                  _ <- assertWith(
//                    msg = "Deposits should be correct with absorbed deposit",
//                    condition = majorBlock.body.depositsAbsorbed == List(depositReq.eventId) &&
//                        majorBlock.body.depositsRefunded == List.empty
//                  )
//
//                  expectedUtxos = L2EventGenesis(
//                    Queue.from(depositRefundTxSeq.depositTx.depositProduced.virtualOutputs.toList),
//                    TransactionHash.fromByteString(
//                      scalus.builtin.platform.blake2b_256(
//                        env.tokenNames.headTokenName.bytes ++
//                            ByteString.fromBigIntBigEndian(
//                              BigInt(BlockVersion.Full.unapply(majorBlock.header.blockVersion)._1)
//                            )
//                      )
//                    )
//                  ).asUtxos
//
//                  _ <- assertWith(
//                    msg = "Virtual Ledger should contain expected active utxo",
//                    condition = jlState.virtualLedgerState.activeUtxos == expectedUtxos
//                  )
//
//                  kzgCommit = jlState.virtualLedgerState.kzgCommitment
//
//                  expectedKzg = KzgCommitment.calculateCommitment(
//                    KzgCommitment.hashToScalar(expectedUtxos)
//                  )
//
//                  _ <- assertWith(
//                    msg =
//                        s"KZG Commitment is correct.\n\tObtained: ${kzgCommit}\n\tExpected: ${expectedKzg}",
//                    condition = kzgCommit == expectedKzg
//                  )
//
//              } yield ()
//
//              // Step 5: Finalize
//              _ <- startBlockNow(BlockNumber.zero.increment.increment.increment.increment)
//              _ <- completeBlockFinal(None)
//          } yield true
//        )
//
//    // TODO: This could probably just be unit tests. We're not generating much of interest. Perhaps we set the
//    // number of tests to 1? Also, these should be adapted into a model test
//    val _ = property("Deposit absorption end Timing Test") = run(
//      initializer = defaultInitializer,
//      testM = for {
//          env <- ask[TestR]
//          blockStartTime <- startBlockNow(BlockNumber.zero.increment)
//
//          // Test: Deposit absorption window ends 1 second before block start time
//          _ <- for {
//              // We need the absorption end to be prior to the start of the block
//              //
//              //    depositAbsorptionEnd = depositValidity_end + deposit_maturity_duration + deposit_absorption_duration
//              //                         = blockStartTime - 1 slot
//              // thus
//              // depositValidity_end = blockStartTime - 1 slot - deposit_maturity_duration - deposit_absorption_duration
//              seqAndReq <- deposit(
//                validityEnd =
//                    blockStartTime - env.txTiming.depositMaturityDuration - env.txTiming.depositAbsorptionDuration -
//                        FiniteDuration(
//                          testTxBuilderCardanoInfo.slotConfig.slotLength,
//                          TimeUnit.MILLISECONDS
//                        ),
//                LedgerEventId(0, 1),
//                blockStartTime
//              )
//              (depositRefundTxSeq, depositReq) = seqAndReq
//              jlState <- unsafeGetProducing
//
//              _ <- assertWith(
//                msg = "Deposit should not be in dapp ledger state",
//                condition = jlState.dappLedgerState.deposits == Queue.empty
//              )
//
//              _ <- assertWith(
//                msg = "Deposit should be in transient fields as invalid",
//                condition = jlState.nextBlockData.events == List((depositReq.eventId, Invalid))
//              )
//          } yield ()
//
//          // Test: Deposit absorption window ends exactly at block start time
//          _ <- for {
//              seqAndReq <- deposit(
//                blockStartTime - env.txTiming.depositMaturityDuration - env.txTiming.depositAbsorptionDuration,
//                LedgerEventId(0, 2),
//                blockStartTime
//              )
//              (depositRefundTxSeq, depositReq) = seqAndReq
//              jlState <- unsafeGetProducing
//
//              _ <- assertWith(
//                msg = "Deposit should be in dapp ledger state",
//                condition = jlState.dappLedgerState.deposits == Queue(
//                  (depositReq.eventId, depositRefundTxSeq.depositTx.depositProduced)
//                )
//              )
//
//              _ <- assertWith(
//                msg = "Deposit should be in transient fields as valid",
//                condition = jlState.nextBlockData.events.last == (depositReq.eventId, Valid)
//              )
//          } yield ()
//      } yield true
//    )
//
//    val _ = property(
//      "Deposit absorption start timing test: absorption starts at block start time, deposit gets absorbed"
//    ) = run(
//      initializer = defaultInitializer,
//      testM = for {
//          env <- ask[TestR]
//          blockStartTime <- startBlockNow(BlockNumber.zero.increment)
//
//          // Test: Deposit absorption window starts exactly at block start time
//          _ <- for {
//              // We need the absorption start to be prior to the start of the block
//              //
//              //    blockStartTime         =
//              //    depositAbsorptionStart = depositValidity_end + deposit_maturity_duration
//              //
//              // thus
//              //
//              //    depositValidityEnd = blockStartTime - deposit_maturity_duration
//              seqAndReq <- deposit(
//                validityEnd = blockStartTime - env.txTiming.depositMaturityDuration,
//                LedgerEventId(0, 1),
//                blockStartTime = blockStartTime
//              )
//              (depositRefundTxSeq, depositReq) = seqAndReq
//              jlState <- unsafeGetProducing
//
//              _ <- assertWith(
//                msg = "Deposit should be in dapp ledger state",
//                condition = jlState.dappLedgerState.deposits == Queue(
//                  (depositReq.eventId, depositRefundTxSeq.depositTx.depositProduced)
//                )
//              )
//
//              _ <- assertWith(
//                msg = "Deposit should be in transient fields as valid",
//                condition = jlState.nextBlockData.events == List((depositReq.eventId, Valid))
//              )
//
//              // Now we complete the block, including this deposit in the poll results.
//              _ <- completeBlockRegular(
//                None,
//                Set(depositRefundTxSeq.depositTx.depositProduced.toUtxo.input)
//              )
//              jlState <- unsafeGetDone
//
//              _ <- assertWith(
//                msg = "Produced block should be Major",
//                condition = jlState.producedBlock.isInstanceOf[Block.Unsigned.Major]
//              )
//
//              majorBlock = jlState.producedBlock.asInstanceOf[Block.Unsigned.Major]
//
//              _ <- assertWith(
//                msg = "Block should contain absorbed deposit",
//                condition = majorBlock.body.depositsAbsorbed == List(depositReq.eventId)
//              )
//          } yield ()
//      } yield true
//    )
//
//    val _ = property(
//      "Deposit absorption start timing test: absorption starts 1 second after block start time"
//    ) = run(
//      initializer = defaultInitializer,
//      testM = for {
//          env <- ask[TestR]
//          blockStartTime <- startBlockNow(BlockNumber.zero.increment)
//
//          // Test: Deposit absorption window starts 1 second after block start time
//          _ <- for {
//              // We want:
//              //    blockStartTime + 1 =
//              //    depositAbsorptionStart = depositValidity_end + deposit_maturity_duration
//              //
//              // thus
//              //
//              //    depositValidityEnd = blockStartTime + 1 - deposit_maturity_duration
//              seqAndReq <- deposit(
//                blockStartTime + 1.seconds - env.txTiming.depositMaturityDuration,
//                LedgerEventId(0, 1),
//                blockStartTime
//              )
//              (depositRefundTxSeq, depositReq) = seqAndReq
//              jlState <- unsafeGetProducing
//
//              _ <- assertWith(
//                msg = "Deposit should be in dapp ledger state",
//                condition = jlState.dappLedgerState.deposits == Queue(
//                  (depositReq.eventId, depositRefundTxSeq.depositTx.depositProduced)
//                )
//              )
//
//              _ <- assertWith(
//                msg = "Deposit should be in transient fields as valid",
//                condition = jlState.nextBlockData.events == List((depositReq.eventId, Valid))
//              )
//
//              // Now we complete the block, including this deposit in the poll results.
//              _ <- completeBlockRegular(
//                None,
//                Set(depositRefundTxSeq.depositTx.depositProduced.toUtxo.input)
//              )
//              jlState <- unsafeGetDone
//
//              _ <- assertWith(
//                msg = "Produced block should be Minor",
//                condition = jlState.producedBlock.isInstanceOf[Block.Unsigned.Minor]
//              )
//
//              minorBlock = jlState.producedBlock.asInstanceOf[Block.Unsigned.Minor]
//
//              // This is trivial statement which may be removed
//              _ <- assertWith(
//                msg = "Block should not contain absorbed deposit",
//                condition = minorBlock.body.depositsAbsorbed == List.empty
//              )
//          } yield ()
//      } yield true
//    )
//
//}
