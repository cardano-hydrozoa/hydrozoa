package hydrozoa.multisig.ledger

import cats.*
import cats.data.*
import cats.effect.*
import cats.effect.unsafe.implicits.*
import cats.implicits.*
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.ActorRef
import com.suprnation.actor.{ActorSystem, test as _}
import hydrozoa.config.head.multisig.timing.TxTiming
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.{BlockCreationEndTime, BlockCreationStartTime}
import hydrozoa.config.head.multisig.timing.TxTiming.RequestTimes.{RequestValidityEndTime, unsafeRequestValidityEndTime}
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.lib.actor.SyncRequest
import hydrozoa.lib.cardano.scalus.QuantizedTime.*
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant.realTimeQuantizedInstant
import hydrozoa.lib.cardano.scalus.ledger.stripVKeyWitnesses
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.consensus.{ConsensusActor, RequestValidityEndTimeRaw, RequestValidityStartTimeRaw, UserRequest, UserRequestBody, UserRequestHeader, UserRequestWithId}
import hydrozoa.multisig.ledger.JointLedgerTestHelpers.*
import hydrozoa.multisig.ledger.JointLedgerTestHelpers.Requests.*
import hydrozoa.multisig.ledger.JointLedgerTestHelpers.Scenarios.*
import hydrozoa.multisig.ledger.block.{Block, BlockBrief, BlockNumber}
import hydrozoa.multisig.ledger.eutxol2.tx.{GenesisObligation, L2Genesis}
import hydrozoa.multisig.ledger.eutxol2.{EutxoL2Ledger, toEvacuationKey}
import hydrozoa.multisig.ledger.event.RequestId.ValidityFlag.{Invalid, Valid}
import hydrozoa.multisig.ledger.event.{RequestId, RequestNumber}
import hydrozoa.multisig.ledger.joint.JointLedger.Requests.{CompleteBlockFinal, CompleteBlockRegular, StartBlock}
import hydrozoa.multisig.ledger.joint.JointLedger.{Done, Producing}
import hydrozoa.multisig.ledger.joint.{EvacuationMap, JointLedger, given}
import hydrozoa.multisig.ledger.l1.deposits.map.DepositsMap
import hydrozoa.multisig.ledger.l1.txseq.DepositRefundTxSeq
import java.util.concurrent.TimeUnit
import monocle.Focus
import monocle.Focus.focus
import org.scalacheck.*
import org.scalacheck.Prop.propBoolean
import org.scalacheck.PropertyM.monadForPropM
import org.scalacheck.util.Pretty
import scala.collection.immutable.Queue
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.{Block as _, BlockHeader as _, Coin, *}
import scalus.crypto.ed25519.Signature
import scalus.uplc.builtin.ByteString
import test.*
import test.Generators.Hydrozoa.*
import test.Generators.Other.genCoinDistributionWithMinAdaUtxo
import test.TestM.*

// Pretty Printers for more manageable scalacheck logs
given ppMultiNodeConfig: (MultiNodeConfig => Pretty) = nodeConfig =>
    Pretty(_ => "MultiNodeConfig (too long to print)")

// TODO: restore? Do we use it?
//given ppTestPeers: (TestPeers => Pretty) = testPeers =>
//    Pretty(_ =>
//        "TestPeers:"
//            + s"\n\t Num Peers: ${testPeers._headPeersName.length}"
//            + (testPeers._headPeersName.map(testPeer =>
//                f"\n\t${testPeer._1.peerNum.toInt}%2d"
//                    + s" | ${testPeers.wallet(testPeer._2).exportVerificationKey.take(2)}(...)"
//                    + s" | ${testPeer._2.name} "
//            ))
//    )

/** This object contains component-specific helpers to utilize the TestM type.
  *
  * It defines the following bits:
  *
  *   - A [[JLTest]] type alias over [[TestM]], to cut down on type signature noise
  *   - A [[TestR]] environment, specific to the joint ledger tests
  *   - A [[defaultInitializer]] that initializers the TestR environment under which the joint
  *     ledger tests run
  *   - A [[Requests]] object that "thinly" lifts [[IO]] actor requests into [[JLTest]], which
  *     simplifies writing monadic code. "Thinly" here means that the defined functions don't
  *     _morally_ add additional effects.
  *     - One exception to this may be testing pre-conditions or post-conditions; for example, every
  *       non-exception-throwing call to `completeBlockRegular` should result transition of a
  *       [[JointLedger.State]] from [[Producing]] to [[Done]], but this can only be accomplished by
  *       sending two additional messages to the JointLedger actor in order to observe the state
  *       before and after. If your test is sensitive to the EXACT messages that are sent to the
  *       actor's mailbox, then you may need to make some modifications.
  *   - A [[Scenarios]] object that includes pre-defined, composable test snippets, making full use
  *     of the embedded PropertyM and IO of the JLTest.
  */
object JointLedgerTestHelpers {

    /** An agent of this test, pretending to be a [[ConsensusActor]] for [[JointLedger]]. */
    final case class ConsensusAgent() extends Actor[IO, ConsensusAgent.Request] {
        private val blocks = Ref.unsafe[IO, ConsensusAgent.State](Vector.empty)

        override def receive: Receive[IO, ConsensusAgent.Request] = {
            case block: Block.Unsigned.Next => blocks.update(_ :+ block)
            case m: ConsensusAgent.GetState.Sync =>
                for {
                    response <- blocks.get
                    _ <- m.dResponse.complete(response)
                } yield ()
            case _ => IO.unit
        }
    }

    object ConsensusAgent {
        type State = Vector[Block.Unsigned.Next]

        case object GetState extends SyncRequest[IO, GetState.type, ConsensusAgent.State] {
            type Sync = SyncRequest.Envelope[IO, GetState.type, ConsensusAgent.State]

            def ?: : this.Send = SyncRequest.send(_, this)
        }

        type Request = ConsensusActor.Request | GetState.Sync
    }

    type JLTest[A] = TestM[TestR, A]
    val defaultInitializer: PropertyM[IO, TestR] = {
        for {
            multiNodeConfig <- PropertyM.pick[IO, MultiNodeConfig](
              MultiNodeConfig.generate(
                TestPeersSpec.default.withPeersNumberSpec(PeersNumberSpec.Exact(1))
              )()
            )

            // testPeers <- PropertyM.pick[IO, TestHeadPeers](generateTestPeers())
            // config <- PropertyM.pick[IO, NodeConfig](
            //  generateNodeConfig(Exact(SeedPhrase.Yaci, testPeers.headPeers.nHeadPeers.toInt))()
            // )

            config = multiNodeConfig.nodeConfigs(HeadPeerNumber.zero)

            system <- PropertyM.run(ActorSystem[IO]("DappLedger").allocated.map(_._1))

            consensusAgent <- PropertyM.run(system.actorOf(ConsensusAgent()))

            eutxoLedger <- PropertyM.run(EutxoL2Ledger(config))
            jointLedger <- PropertyM.run(
              system.actorOf(
                JointLedger(
                  config,
                  JointLedger.Connections(
                    consensusActor = consensusAgent.narrowRequest[ConsensusActor.Request],
                    peerLiaisons = List()
                  ),
                  eutxoLedger,
                  hydrozoa.lib.tracing.ProtocolTracer.noop
                )
              )
            )
        } yield TestR(
          multiNodeConfig = multiNodeConfig,
          config = config,
          actorSystem = system,
          jointLedger = jointLedger,
          consensusAgent = consensusAgent
        )
    }

    /** The "environment" that is contained in the ReaderT of the JLTest
      */
    case class TestR(
        multiNodeConfig: MultiNodeConfig,
        config: JointLedger.Config,
        actorSystem: ActorSystem[IO],
        jointLedger: ActorRef[IO, JointLedger.Requests.Request],
        consensusAgent: ActorRef[IO, ConsensusAgent.Request]
    )

    /** Helper utilities to send actor Requests to the JointLedger
      */
    object Requests {
        val getJointLedgerState: JLTest[JointLedger.State] =
            for {
                env <- ask
                state <- lift(env.jointLedger ?: JointLedger.Requests.GetState)
            } yield state

        val getConsensusAgentState: JLTest[ConsensusAgent.State] =
            for {
                env <- ask
                state <- lift(env.consensusAgent ?: ConsensusAgent.GetState)
            } yield state

        def registerDeposit(req: UserRequestWithId): JLTest[Unit] = {
            for {
                jl <- asks[TestR, ActorRef[IO, JointLedger.Requests.Request]](_.jointLedger)
                _ <- lift(jl ? req)
            } yield ()
        }

        /** Start the block at the current real time */
        def startBlockNow(blockNum: BlockNumber): JLTest[BlockCreationStartTime] =
            for {
                env <- ask
                now <- lift(
                  realTimeQuantizedInstant(env.config.slotConfig).map(BlockCreationStartTime)
                )
                _ <- startBlock(blockNum, now)
            } yield now

        def startBlock(
            blockNum: BlockNumber,
            blockCreationTime: BlockCreationStartTime,
        ): JLTest[Unit] =
            startBlock(StartBlock(blockNum, blockCreationTime))

        def startBlock(req: StartBlock): JLTest[Unit] =
            ask.flatMap(env => lift(env.jointLedger ! req))

        def completeBlockRegular(
            referenceBlock: Option[BlockBrief.Intermediate],
            blockCreationEndTime: BlockCreationEndTime,
            pollResults: Set[TransactionInput]
        ): JLTest[Unit] =
            for {
                env <- ask
                _ <- completeBlockRegular(
                  CompleteBlockRegular(
                    referenceBlock,
                    pollResults: Set[TransactionInput],
                    false,
                    blockCreationEndTime
                  )
                )
            } yield ()

        /** WARNING: This method performs pre-and-post condition checks on the joint ledger. This
          * means two things:
          *   - This will send three messages to the JointLedger -- two to check the state before
          *     and after, and one to actually send the CompleteBlockRegular request
          *   - There is a race condition if this function is used in a context where multiple
          *     messages are being sent to the JointLedger from different sources
          *
          * I (Peter) am trying this out for now (2026-01-08), but I suspect I'll want a better way
          * to do this in the future -- feel free to remove these checks if they're annoying.
          * Perhaps hacking something into to cats-actors so that I can send multiple messages that
          * I know will be processed as a batch is a way forward?
          */
        def completeBlockRegular(req: CompleteBlockRegular): JLTest[Unit] =
            for {
                jl <- asks[TestR, ActorRef[IO, JointLedger.Requests.Request]](_.jointLedger)
                beforeState <- lift(jl ?: JointLedger.Requests.GetState)
                _ <- lift(jl ! req)
                afterState <- lift(jl ?: JointLedger.Requests.GetState)
                _ <- assertWith[TestR](
                  condition = beforeState.isInstanceOf[JointLedger.Producing],
                  msg = "A CompleteBlockRegular request was sent to the JointLedger and it succeeded, but" +
                      " the JointLedger wasn't in the Producing state before the request was sent"
                )
                _ <- assertWith[TestR](
                  condition = afterState.isInstanceOf[JointLedger.Done],
                  msg = "A CompleteBlockRegular request was sent to the JointLedger and it succeeded, but" +
                      " the JointLedger didn't transition to the Done state after the request was processed"
                )
            } yield ()

        def completeBlockFinal(
            referenceBlock: Option[BlockBrief.Final],
        ): JLTest[Unit] =
            for {
                env <- ask
                now <- lift(
                  realTimeQuantizedInstant(env.config.slotConfig).map(BlockCreationEndTime)
                )
                _ <- completeBlockFinal(
                  CompleteBlockFinal(referenceBlock, now)
                )
            } yield ()

        def completeBlockFinal(req: CompleteBlockFinal): JLTest[Unit] =
            for {
                jl <- asks[TestR, ActorRef[IO, JointLedger.Requests.Request]](_.jointLedger)
                _ <- lift(jl ! req)
            } yield ()

    }

    /** Helper utilities to execute particular scenarios, such as "generating a random deposit and
      * sending it to the JointLedger"
      */
    object Scenarios {

        def unsafeGetProducing: JLTest[Producing] =
            for {
                state <- getJointLedgerState
                p <- state match {
                    case _: Done => throw RuntimeException("Expected a Producing State, got Done")
                    case p: Producing => TestM.pure[TestR, Producing](p)
                }
            } yield p

        def unsafeGetDone: JLTest[Done] =
            for {
                state <- getJointLedgerState
                d <- state match {
                    case d: Done => TestM.pure[TestR, Done](d)
                    case _: Producing =>
                        throw RuntimeException("Expected a Done State, got Producing")
                }
            } yield d

        /** Generate a random (sensible) deposit from a random peer and send it to the joint ledger
          */
        def deposit(
            requestValidityEndTime: RequestValidityEndTime,
            requestId: RequestId,
            blockCreationStartTime: BlockCreationStartTime
        ): JLTest[
          (DepositRefundTxSeq, UserRequestWithId, NonEmptyList[GenesisObligation])
        ] = {
            import Requests.*
            for {
                env <- ask[TestR]

                address = env.multiNodeConfig.addressOf(HeadPeerNumber.zero)

                l2Outputs <-
                    pick[TestR, NonEmptyList[GenesisObligation]](
                      (
                        for {
                            numL2Outputs <- Gen.choose(1, 100)
                            res <- Gen.listOfN(
                              numL2Outputs,
                              genGenesisObligation(env.config, address, minimumCoin = Coin.ada(5))
                            )
                        } yield res
                      ).map(NonEmptyList.fromListUnsafe)
                          .label(s"L2 Outputs for deposit $requestId")
                    )

                l2OutputsBytes = GenesisObligation.serialize(l2Outputs)

                l2OutputsValue = Value.combine(
                  l2Outputs.map(vo => Value(vo.l2OutputValue)).toList
                )

                utxosFunding <- pick[TestR, NonEmptyList[Utxo]]((for {
                    numUtxos <- Gen.choose(1, 100)
                    utxosWith0Coin <- Gen
                        .listOfN(
                          numUtxos,
                          genAdaOnlyPubKeyUtxo(env.config, address, minimumCoin = Coin.ada(3))
                        )
                    utxoDist <- genCoinDistributionWithMinAdaUtxo(
                      l2OutputsValue.coin,
                      NonEmptyList.fromListUnsafe(utxosWith0Coin),
                      env.config.cardanoProtocolParams
                    )
                } yield utxoDist).label("Funding Utxos"))

                utxosFundingValue = Value.combine(utxosFunding.toList.map(_._2.value))

                depositRefundSeqBuilder = DepositRefundTxSeq.Build(env.config)(
                  l2Payload = l2OutputsBytes,
                  l2Value = l2OutputsValue,
                  depositFee = Coin.zero,
                  utxosFunding = utxosFunding,
                  changeAddress = address,
                  requestValidityEndTime = requestValidityEndTime,
                  refundAddress = address,
                  refundDatum = None,
                  requestId = requestId
                )

                depositRefundTxSeq <- lift(depositRefundSeqBuilder.result.liftTo[IO])

                // refund(i).validity_start = deposit(i).absorption_end + silence_period
                // refund(i).validity_end = ∅
                _ <- assertWith[TestR](
                  msg = "refund start validity is correct",
                  condition = {
                      depositRefundTxSeq.refundTx.tx.body.value.validityStartSlot.isDefined
                      && Slot(depositRefundTxSeq.refundTx.tx.body.value.validityStartSlot.get)
                          .toQuantizedInstant(env.config.slotConfig)
                          ==
                          env.config.txTiming.refundValidityStart(requestValidityEndTime)
                  }
                )

                _ <- assertWith[TestR](
                  msg = "refund end validity is correct",
                  condition = depositRefundTxSeq.refundTx.tx.body.value.ttl.isEmpty
                )

                signTx = env.multiNodeConfig.signTxAs(HeadPeerNumber.zero)
                body: UserRequestBody.DepositRequestBody = UserRequestBody.DepositRequestBody(
                  l1Payload = ByteString.fromArray(depositRefundTxSeq.depositTx.tx.toCbor),
                  l2Payload = GenesisObligation.serialize(l2Outputs)
                )

                header = UserRequestHeader(
                  headId = env.config.headId,
                  validityStart =
                      RequestValidityStartTimeRaw(blockCreationStartTime.getEpochSecond),
                  validityEnd = RequestValidityEndTimeRaw(
                    depositRefundTxSeq.depositTx.depositProduced.requestValidityEndTime.getEpochSecond
                  ),
                  bodyHash = body.hash
                )

                userWallet = env.multiNodeConfig
                    .nodePrivateConfigs(HeadPeerNumber.zero)
                    .ownHeadPeerPrivate
                    .ownHeadWallet

                userVk = userWallet.exportVerificationKey

                signature: Signature =
                    Signature.unsafeFromArray(
                      IArray.genericWrapArray(userWallet.signMsg(IArray.from(header.bytes))).toArray
                    )

                Right(request) = UserRequest.DepositRequest(
                  header = header,
                  body = body,
                  userVk = userVk,
                  signature = signature
                ): @unchecked

                req =
                    UserRequestWithId(
                      userRequest = request,
                      requestId = requestId
                    )

                _ <- registerDeposit(req)
            } yield (depositRefundTxSeq, req, l2Outputs)
        }
    }
}

// Annoyingly, `Gen` doesn't have `Monad[Gen]` already. But I want to use `traverse` below, so I'm vendoring it here
implicit val genMonad: Monad[Gen] = new Monad[Gen] {
    def pure[A](a: A): Gen[A] = Gen.const(a)
    def flatMap[A, B](fa: Gen[A])(f: A => Gen[B]): Gen[B] = fa.flatMap(f)
    def tailRecM[A, B](a: A)(f: A => Gen[Either[A, B]]): Gen[B] = Gen.tailRecM(a)(f)
}

object JointLedgerTest extends Properties("Joint Ledger Test") {
//    import org.scalacheck.rng.Seed
//    override def overrideParameters(p: Test.Parameters): Test.Parameters =
//        p.withInitialSeed(Seed.fromBase64("zeNyOvJsm4OxLrnAPz9TE8ooirzE1AR-Zsof63oHXHB=").get)

    import TestM.*

    //  We can observe three test statistics:
    //  - Whether ties are occurring
    //  - Whether the sorting is trivial due to events being pre-sorted
    //  - Whether the sorting is trivial due < 2 events
    //
    //  The properties we check:
    //    - Deposits are indeed sorted according to start time
    //    - Sorting does not change the number of elements
    //    - The sequences of deposits grouped by the same start times are all sub-sequences of the unsorted stream.
    val _ = property("deposit sorting") = run(
      // FIXME: initializer is too bulky, we can reduce it significantly
      initializer = defaultInitializer,
      testM = {
          def genEventStream(
              config: CardanoNetwork.Section & TxTiming.Section,
              headAddress: ShelleyAddress,
              blockCreationStartTime: BlockCreationStartTime,
              blockCreationEndTime: BlockCreationEndTime
          ): Gen[
            Queue[JLTest[
              (
                  DepositRefundTxSeq,
                  UserRequestWithId,
                  NonEmptyList[GenesisObligation]
              )
            ]]
          ] =
              for {
                  lastRequestIds: Map[HeadPeerNumber, RequestNumber] <- for {
                      n <- Gen.choose(2, 20)
                      headPeerNumbers = Vector.range(0, n).map(HeadPeerNumber(_))
                      tuples <- headPeerNumbers
                          .map(peerNum =>
                              Gen.posNum[Int]
                                  .map(requestNum => (peerNum, RequestNumber(requestNum)))
                          )
                          .sequence
                  } yield Map.from(tuples)

                  events <- Gen.tailRecM(
                    (
                      lastRequestIds,
                      Queue.empty[JLTest[
                        (
                            DepositRefundTxSeq,
                            UserRequestWithId,
                            NonEmptyList[GenesisObligation]
                        )
                      ]]
                    )
                  )(
                    (
                        lastRequestIds: Map[HeadPeerNumber, RequestNumber],
                        actionQueue: Queue[JLTest[
                          (
                              DepositRefundTxSeq,
                              UserRequestWithId,
                              NonEmptyList[GenesisObligation]
                          )
                        ]]
                    ) =>
                        // In the recursive case: pick a random peer, increment their event number, and generate a
                        // deposit action for them
                        lazy val appendEvent: Gen[
                          (
                              Map[HeadPeerNumber, RequestNumber],
                              Queue[JLTest[
                                (
                                    DepositRefundTxSeq,
                                    UserRequestWithId,
                                    NonEmptyList[GenesisObligation]
                                )
                              ]]
                          )
                        ] =
                            for {
                                peer <- Gen.oneOf(lastRequestIds.keys)
                                lastRequestID = lastRequestIds(peer)
                                newRequestIds = lastRequestIds.updated(
                                  peer,
                                  lastRequestID.increment
                                )

                                requestValidityEndTimeOffset <- Gen
                                    .choose(
                                      1,
                                      (blockCreationEndTime - blockCreationStartTime).finiteDuration.toSeconds.toInt
                                    )
                                    .map(_.seconds)
                                    .map(QuantizedFiniteDuration(config.slotConfig, _))

                            } yield (
                              newRequestIds,
                              actionQueue.appended(
                                deposit(
                                  requestValidityEndTime = unsafeRequestValidityEndTime(
                                    blockCreationStartTime + requestValidityEndTimeOffset
                                  ),
                                  requestId = RequestId(peer, lastRequestID.increment),
                                  blockCreationStartTime = blockCreationStartTime
                                )
                              )
                            )
                        Gen.frequency(
                          (1, Gen.const(Right(actionQueue))),
                          (10, appendEvent.map(Left(_)))
                        )
                  )
              } yield events

//          @tailrec
//          def isSubsequenceOf[A](sub: Seq[A], seq: Seq[A]): Boolean = {
//              if sub.isEmpty then true
//              else
//                  Try(seq.dropWhile(_ != sub.head).tail) match {
//                      case Success(nextSeq) => isSubsequenceOf(sub.tail, nextSeq)
//                      case Failure(_)       => false
//                  }
//          }
          /** order-preserving, non-contiguous matching
            * {{{
            * isSubsequenceOf( List(1, 2, 3), List(1, 6, 3, 2, 3, 7, 1)) == true
            * }}}
            */
          def isSubsequenceOf[A](sub: Seq[A], seq: Seq[A]) = {
              val filtered = seq.filter(sub.contains(_))
              filtered == sub
          }

          for {
              env <- ask[TestR]
              blockStartTime <- startBlockNow(BlockNumber.zero.increment)
              eventStreamActions <- pick[TestR, Queue[
                JLTest[
                  (
                      DepositRefundTxSeq,
                      UserRequestWithId,
                      NonEmptyList[GenesisObligation]
                  )
                ]
              ]](
                genEventStream(
                  env.config,
                  env.config.headMultisigAddress,
                  blockStartTime,
                  BlockCreationEndTime(blockStartTime + 10.seconds)
                )
              )

              eventStreamFullResults <- eventStreamActions.sequence
              // This is the format we actually care about; it's commensurate with the DappLedgerState
              eventStream: Queue[DepositsMap.Entry] = eventStreamFullResults.map {
                  case (txSeq, event, obligations) =>
                      DepositsMap.Entry(event.requestId, txSeq.depositTx.depositProduced)
              }

              depositsMap <- getJointLedgerState.map(_.l1LedgerState.deposits)

              // Test statistic:  make sure that ties are actually occurring in some samples
              _ <- lift[TestR, Unit](PropertyM.monitor[IO](Prop.collect {
                  if eventStream.length <= 1
                  then "events.length <= 1"
                  else "events.length > 1"
              }))

              // Test statistic:  make sure that ties are actually occurring in some samples
              _ <- lift[TestR, Unit](PropertyM.monitor[IO](Prop.collect {
                  val collectionSizes = depositsMap.treeMap.map(_._2.length)
                  if collectionSizes.forall(_ == 1)
                  then "no duplicate start times"
                  else "some duplicate start times"
              }))

              // Test statistic: the flattened deposits map and unsorted stream are different
              _ <- lift[TestR, Unit](PropertyM.monitor[IO](Prop.collect {
                  if depositsMap.flatten == eventStream
                  then "depositsMap.flatValues == eventStream"
                  else "depositsMap.flatValues != eventStream"
              }))

              _ <- assertWith[TestR](
                msg = "Deposits are sorted by absorption start time",
                condition = {
                    val startTimes = depositsMap.depositUtxos.map(deposit =>
                        env.config.txTiming
                            .depositAbsorptionStartTime(
                              deposit.requestValidityEndTime
                            )
                            .convert
                    )
                    startTimes.toList.sorted == startTimes
                }
              )

              _ <- assertWith[TestR](
                msg =
                    "Deposit ledger state includes the same number of elements as the event stream",
                condition = depositsMap.numberOfDeposits == eventStream.length
              )

              _ <- assertWith[TestR](
                msg =
                    "If multiple deposits have the same absorption start time, order of the sorted deposits must be" +
                        " a subsequence of the event stream",
                condition = {
                    depositsMap.treeMap.values
                        .forall(eventQueuesByStartTime =>
                            isSubsequenceOf(eventQueuesByStartTime, eventStream)
                        )
                }
              )
          } yield true

      }
    )

    val _ = property("Joint Ledger Happy Path") = run(
      initializer = defaultInitializer,
      testM = for {
          env <- ask[TestR]

          // Put the joint ledger in producing mode
          firstBlockNumber = BlockNumber.zero.increment
          firstBlockCreationStartTime <- startBlockNow(firstBlockNumber)
          firstBlockCreationEndTime = BlockCreationEndTime(firstBlockCreationStartTime + 20.seconds)
          // Generate a deposit and observe that it appears in the dapp ledger correctly
          firstDepositValidityEnd = unsafeRequestValidityEndTime(
            firstBlockCreationEndTime + 10.minutes
          )
          seqAndReq <- deposit(
            requestValidityEndTime = firstDepositValidityEnd,
            requestId = RequestId(0, 1),
            blockCreationStartTime = firstBlockCreationStartTime
          )
          (depositRefundTxSeq, depositReq, genesisObligations) = seqAndReq
          depositProduced = depositRefundTxSeq.depositTx.depositProduced

          _ <- for {
              jlState <- getJointLedgerState
              dlState = jlState.l1LedgerState

              _ <- assertWith[TestR](
                msg =
                    s"We should have 1 deposit in the state. We have ${dlState.deposits.numberOfDeposits}",
                condition = dlState.deposits.numberOfDeposits == 1
              )
              _ <- assertWith[TestR](
                msg = "Correct deposit(s) in state",
                condition = dlState.deposits.depositUtxos.head == depositProduced
              )
              _ <- assertWith[TestR](
                msg = "Correct treasury in state",
                condition = dlState.treasury == env.config.initializationTx.treasuryProduced
              )
              _ <- assertWith[TestR](
                msg = "Correct refund in state",
                condition =
                    val refundsWithoutSignatures =
                        jlState
                            .asInstanceOf[Producing]
                            .userRequestState
                            .postDatedRefundTxs
                            // Zero out the vkey witnesses before checking equality
                            .map(_.focus(_.tx).modify(_.stripVKeyWitnesses))
                    refundsWithoutSignatures == Vector(depositRefundTxSeq.refundTx)
              )
          } yield ()

          // Complete a block, but assume the deposit didn't show up in the poll results
          _ <- completeBlockRegular(None, firstBlockCreationEndTime, Set.empty)
          _ <-
              for {
                  consensusAgentState <- getConsensusAgentState
                  _ <- assertWith[TestR](
                    msg = "Joint ledger should only have sent out one block, total.",
                    condition = consensusAgentState.size == 1
                  )

                  block = consensusAgentState(0)

                  _ <- assertWith[TestR](
                    msg = "First block should be minor -- no deposits/withdrawals.",
                    condition = block.isInstanceOf[Block.Unsigned.Minor]
                  )

                  _ <- assertWith[TestR](
                    msg = "Block's deposit absorbed and deposits refunded should both be empty",
                    condition = block.body.depositsRefunded.isEmpty
                        && block.body.depositsAbsorbed.isEmpty
                  )
                  _ <- assertWith[TestR](
                    msg = "Post-dated refund should appear",
                    condition = block.effects.postDatedRefundTxs
                        .map(_.focus(_.tx).modify(_.stripVKeyWitnesses)) == List(
                      depositRefundTxSeq.refundTx
                    )
                  )
              } yield ()

          // Complete another block, assume the deposit shows up in the poll results -- but it's not mature yet
          secondBlockNumber = firstBlockNumber.increment
          secondBlockCreationStartTime = BlockCreationStartTime(
            firstBlockCreationEndTime + 5.seconds
          )
          secondBlockCreationEndTime = BlockCreationEndTime(
            secondBlockCreationStartTime + 20.seconds
          )

          secondBlockCreationStartTime <- startBlock(
            secondBlockNumber,
            secondBlockCreationStartTime
          )
          _ <- completeBlockRegular(
            None,
            secondBlockCreationEndTime,
            Set(depositProduced.toUtxo.input)
          )
          _ <- for {
              consensusAgentState <- getConsensusAgentState
              _ <- assertWith[TestR](
                msg = "Joint ledger should only have sent out two block, total.",
                condition = consensusAgentState.size == 2
              )

              block = consensusAgentState(1)

              _ <- assertWith[TestR](
                msg = "Second block should be minor -- no deposits were absorbed.",
                condition = block.isInstanceOf[Block.Unsigned.Minor]
              )
          } yield ()

          // Complete another block, including the deposit in the state.
          thirdBlockNumber = secondBlockNumber.increment
          thirdBlockCreationStartTime = BlockCreationStartTime(depositProduced.absorptionStartTime)
          thirdBlockCreationEndTime = BlockCreationEndTime(thirdBlockCreationStartTime + 20.seconds)
          _ <- startBlock(thirdBlockNumber, thirdBlockCreationStartTime)
          _ <- completeBlockRegular(
            None,
            thirdBlockCreationEndTime,
            Set(depositProduced.toUtxo.input)
          )

          _ <- for {
              jlState <- getJointLedgerState

              consensusAgentState <- getConsensusAgentState
              _ <- assertWith[TestR](
                msg = "Joint ledger should only have sent out three block, total.",
                condition = consensusAgentState.size == 3
              )

              block = consensusAgentState(2)

              _ <- assertWith[TestR](
                msg = "Third block should be major.",
                condition = block.isInstanceOf[Block.Unsigned.Major]
              )

              majorBlock = block.asInstanceOf[Block.Unsigned.Major]

              _ <- assertWith[TestR](
                msg = "Deposits should be correct with absorbed deposit",
                condition = majorBlock.body.depositsAbsorbed == List(depositReq.requestId) &&
                    majorBlock.body.depositsRefunded == List.empty
              )

              // Expected Evac map: The genesis utxos from the deposit + the initial l2 set
              initialEvacMap = env.config.initialEvacuationMap.evacuationMap
              depositEvacMap = L2Genesis(
                genesisObligations = Queue.from(genesisObligations.toList),
                genesisId = L2Genesis.mkGenesisId(
                  depositRefundTxSeq.depositTx.depositProduced.utxoId
                )
              ).asUtxos.map((ti, krto) => (ti.toEvacuationKey, krto))

              expectedEvacMap = EvacuationMap(initialEvacMap ++ depositEvacMap)

              _ <- assertWith[TestR](
                msg = "Evacuation map should contain deposit",
                condition = jlState.evacuationMap == expectedEvacMap
              )

              kzgCommit = jlState.evacuationMap.kzgCommitment

              expectedKzg = expectedEvacMap.kzgCommitment

              _ <- assertWith[TestR](
                msg =
                    s"KZG Commitment is correct.\n\tObtained: $kzgCommit\n\tExpected: $expectedKzg",
                condition = kzgCommit == expectedKzg
              )

          } yield ()

          // Step 5: Finalize
          _ <- startBlockNow(BlockNumber.zero.increment.increment.increment.increment)
          _ <- completeBlockFinal(None)
      } yield true
    )

    val _ = property("Accepts deposit registration with sensible submission deadline") = run(
      initializer = defaultInitializer,
      testM = for {
          env <- ask[TestR]
          blockStartTime <- startBlockNow(BlockNumber.zero.increment)

          _ <- for {

              // Sensible stands for the opposite for SubmissionPeriodIsOver error
              requestValidityEndTime <- pick[TestR, RequestValidityEndTime](
                Gen.choose(1, 10)
                    .map(s =>
                        unsafeRequestValidityEndTime(
                          blockStartTime + FiniteDuration(s, TimeUnit.SECONDS)
                        )
                    )
              )

              seqAndReq <- deposit(
                requestValidityEndTime = requestValidityEndTime,
                RequestId(0, 1),
                blockStartTime
              )

              (depositRefundTxSeq, depositReq, genesisObligations) = seqAndReq
              jlState <- unsafeGetProducing

              _ <- assertWith[TestR](
                msg = "Deposit should be in dapp ledger state",
                condition = jlState.l1LedgerState.deposits == DepositsMap.empty.append(
                  DepositsMap.Entry(
                    depositReq.requestId,
                    depositRefundTxSeq.depositTx.depositProduced
                  )
                )
              )

              _ <- assertWith[TestR](
                msg = "Deposit should be in transient fields as valid",
                condition = jlState.userRequestState.requests == List((depositReq.requestId, Valid))
              )
          } yield ()
      } yield true
    )

    val _ = property("Rejects deposit registration with expired submission deadline") = run(
      initializer = defaultInitializer,
      testM = for {
          env <- ask[TestR]
          blockStartTime <- startBlockNow(BlockNumber.zero.increment)

          _ <- for {

              // Sensible stands for the opposite for SubmissionPeriodIsOver error
              requestValidityEndTime <- pick[TestR, RequestValidityEndTime](
                Gen.choose(0, 10)
                    .map(s =>
                        unsafeRequestValidityEndTime(
                          blockStartTime - FiniteDuration(s, TimeUnit.SECONDS)
                        )
                    )
              )

              seqAndReq <- deposit(
                requestValidityEndTime = requestValidityEndTime,
                RequestId(0, 1),
                blockStartTime
              )

              (depositRefundTxSeq, depositReq, genesisObligation) = seqAndReq
              jlState <- unsafeGetProducing

              _ <- assertWith[TestR](
                msg = "Deposit should not be in dapp ledger state",
                condition = jlState.l1LedgerState.deposits.isEmpty
              )

              _ <- assertWith[TestR](
                msg = "Deposit should be in transient fields as invalid",
                condition =
                    jlState.userRequestState.requests == List((depositReq.requestId, Invalid))
              )
          } yield ()
      } yield true
    )

}
