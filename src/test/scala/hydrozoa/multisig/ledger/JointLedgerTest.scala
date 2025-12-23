package hydrozoa.multisig.ledger

import cats.*
import cats.data.*
import scalus.builtin.{ByteString, platform}
import cats.effect.*
import cats.effect.unsafe.implicits.*
import cats.syntax.all.*
import com.suprnation.actor.ActorRef.ActorRef
import com.suprnation.actor.{ActorSystem, test as _}
import hydrozoa.config.EquityShares
import hydrozoa.maxNonPlutusTxFee
import hydrozoa.multisig.ledger.DappLedger.Requests.{GetState, RegisterDeposit}
import hydrozoa.multisig.ledger.JointLedger.Requests.{CompleteBlockRegular, StartBlock}
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.tx.InitializationTx.SpentUtxos
import hydrozoa.multisig.ledger.dapp.tx.{Tx, minInitTreasuryAda}
import hydrozoa.multisig.ledger.dapp.txseq.{DepositRefundTxSeq, InitializationTxSeq}
import hydrozoa.multisig.ledger.dapp.utxo.DepositUtxo
import hydrozoa.multisig.ledger.virtual.{GenesisObligation, L2EventGenesis}
import hydrozoa.multisig.ledger.virtual.commitment.KzgCommitment
import hydrozoa.multisig.protocol.types.*
import hydrozoa.multisig.protocol.types.Block.Version.Full
import hydrozoa.rulebased.ledger.dapp.tx.genEquityShares
import org.scalacheck.*
import org.scalacheck.Prop.propBoolean
import scalus.builtin.ByteString
import scalus.cardano.address.ShelleyPaymentPart.Key
import scalus.cardano.ledger.{Block as _, *}
import scalus.compiler.sir.SIRBuiltins.blake2b_256
import scalus.prelude.Option as SOption
import scalus.testing.kit.TestUtil
import test.Generators.Hydrozoa.*
import test.PrettyGivens.given
import test.*

import scala.concurrent.duration.{FiniteDuration, SECONDS}


object JointLedgerTest extends Properties("Joint Ledger Test") {

  import org.scalacheck.PropertyM.*

  override def overrideParameters(p: Test.Parameters): Test.Parameters = {
    p
      .withMinSuccessfulTests(1)
  }

  type TestError =
    InitializationTxSeq.Builder.Error
      | DepositRefundTxSeq.Builder.Error
      | DappLedger.Errors.RegisterDepositError

  type ET[A] = EitherT[IO, TestError, A]

  
//  type TestM = PropertyM[ReaderT[IntializedTestEnvironment, EitherT[IO, TestError]] A]
  
  // Accept "any" as the error
  def runner(mProp: ET[Prop]): Prop =
    Prop.secure(mProp.value.unsafeRunSync() match {
      case Left(e) =>
        s"Failed: $e" |: false
      case Right(p) => p
    })
    
  case class InitializedTestEnvironment(
                                         // contained in config 
                                         hns: HeadMultisigScript,
                                         peers: NonEmptyList[TestPeer],
                                         actorSystem: ActorSystem[IO],
                                         initTx: InitializationTxSeq,  // Move to HeadConfig
                                         config: Tx.Builder.Config, // Move to HeadConfig
                                         virtualLedger: ActorRef[IO, VirtualLedger.Request],
                                         dappLedger: ActorRef[IO, DappLedger.Requests.Request],
                                         jointLedger: ActorRef[IO, JointLedger.Requests.Request],
                                       )

  val initializationScenario: PropertyM[ET, InitializedTestEnvironment] =
    for {
      peers <- pick[ET, NonEmptyList[TestPeer]](genTestPeers.label("Test Peers"))

      // We make sure that the seed utxo has at least enough for the treasury and multisig witness UTxO, plus
      // a max non-plutus fee
      seedUtxo <- pick[ET, Utxo](
        genAdaOnlyPubKeyUtxo(
          peers.head,
          minimumCoin = minInitTreasuryAda
            + Coin(maxNonPlutusTxFee(testProtocolParams).value * 2)
        ).map(x => Utxo(x._1, x._2)).label("Initialization: seed utxo")
      )

      otherSpentUtxos <- pick[ET, List[Utxo]](
        Gen
          .listOf(genAdaOnlyPubKeyUtxo(peers.head))
          .map(_.map(x => Utxo(x._1, x._2)))
          .label("Initialization: other spent utxos")
      )

      spentUtxos = NonEmptyList(seedUtxo, otherSpentUtxos)

      // Initial deposit must be at least enough for the minAda of the treasury, and no more than the
      // sum of the seed utxos, while leaving enough left for the estimated fee and the minAda of the change
      // output
      initialDeposit <- pick[ET, Coin](
        Gen
          .choose(
            minInitTreasuryAda.value,
            sumUtxoValues(spentUtxos.toList).coin.value
              - maxNonPlutusTxFee(testTxBuilderEnvironment.protocolParams).value
              - minPubkeyAda().value
          )
          .map(Coin(_))
          .label("Initializtion: initial deposit")
      )

      initTxArgs =
        InitializationTxSeq.Builder.Args(
          spentUtxos = SpentUtxos(seedUtxo, otherSpentUtxos),
          initialDeposit = initialDeposit,
          peers = peers.map(_.wallet.exportVerificationKeyBytes),
          env = testTxBuilderEnvironment,
          evaluator = testEvaluator,
          validators = nonSigningValidators,
          initializationTxChangePP =
            Key(AddrKeyHash.fromByteString(ByteString.fill(28, 1.toByte))),
          tallyFeeAllowance = Coin.ada(2),
          votingDuration = 100
        )

      hns = HeadMultisigScript(peers.map(_.wallet.exportVerificationKeyBytes))

      system <- run(
        EitherT.right[TestError](ActorSystem[IO]("DappLedger").allocated.map(_._1))
      )
      initTx <- run(
        EitherT
          .fromEither[IO](InitializationTxSeq.Builder.build(initTxArgs))
          .leftWiden[TestError]
      )

      config = Tx.Builder.Config(
        headNativeScript = hns,
        multisigRegimeUtxo = initTx.initializationTx.multisigRegimeWitness,
        tokenNames = initTx.initializationTx.tokenNames,
        env = TestUtil.testEnvironment,
        evaluator = testEvaluator,
        validators = nonSigningValidators
      )

      virtualLedgerConfig = VirtualLedger.Config(
        slotConfig = config.env.slotConfig,
        slot = 0,
        protocolParams = config.env.protocolParams,
        network = testNetwork
      )
      virtualLedger <- run(
        EitherT.right[TestError](system.actorOf(VirtualLedger(virtualLedgerConfig)))
      )

      dappLedger <- run(
        EitherT.right[TestError](
          system.actorOf(
            DappLedger.create(initTx.initializationTx, config, virtualLedger)
          )
        )
      )

      equityShares <- pick[ET, EquityShares](genEquityShares(peers).label("Equity shares"))

      jointLedger <- run(EitherT.right[TestError](
        system.actorOf(
          JointLedger(dappLedger = dappLedger,
            virtualLedger = virtualLedger,
            peerLiaisons = Seq.empty,
            tallyFeeAllowance = Coin.ada(2),
            initialBlockTime = FiniteDuration(0, SECONDS), // FIXME: Generate
            initialBlockKzg = KzgCommitment.empty,
            equityShares = equityShares,
            multisigRegimeUtxo = config.multisigRegimeUtxo,
            votingDuration = 0,
            treasuryTokenName = config.tokenNames.headTokenName)
        )
      ))

    } yield InitializedTestEnvironment(
      hns,
      peers,
      system,
      initTx,
      config,
      virtualLedger,
      dappLedger,
      jointLedger
    )

  def depositScenario(e: InitializedTestEnvironment): PropertyM[ET, (DepositRefundTxSeq, RegisterDeposit)] = {
    import e.*
    val peer = peers.head
    for {
      virtualOutputs <-
        pick[ET, NonEmptyList[GenesisObligation]](
          Gen.nonEmptyListOf(genGenesisObligation(peer, minimumCoin = Coin.ada(5)))
            .map(NonEmptyList.fromListUnsafe)
            .label("Virtual Outputs")
        )

      virtualOutputsValue = Value.combine(
        virtualOutputs.map(vo => Value(vo.l2OutputValue)).toList
      )

      utxosFundingTail <- pick[ET, List[Utxo]](
        Gen
          .listOf(
            genAdaOnlyPubKeyUtxo(peer, minimumCoin = Coin.ada(5))
          )
          .label("Funding Utxos: Tail")
      )

      utxosFundingHead <- pick[ET, Utxo](
        genAdaOnlyPubKeyUtxo(
          peer,
          minimumCoin = virtualOutputsValue.coin
        ).label("Funding Utxos: Head")
      )

      utxosFunding = NonEmptyList(utxosFundingHead, utxosFundingTail)

      utxosFundingValue = Value.combine(utxosFunding.toList.map(_._2.value))

      depositRefundSeqBuilder = DepositRefundTxSeq.Builder(
        config = config,
        refundInstructions = DepositUtxo.Refund.Instructions(
          LedgerToPlutusTranslation.getAddress(peer.address()),
          SOption.None,
          100
        ),
        donationToTreasury = Coin.zero,
        refundValue = virtualOutputsValue,
        virtualOutputs = virtualOutputs,
        changeAddress = peer.address(),
        utxosFunding = utxosFunding
      )

      depositRefundTxSeq <- run(EitherT.fromEither[IO](depositRefundSeqBuilder.build))

      signedTx = signTx(peer, depositRefundTxSeq.depositTx.tx)

      serializedDeposit = signedTx.toCbor
      req =
        RegisterDeposit(
          serializedDeposit = serializedDeposit,
          virtualOutputs = virtualOutputs,
          eventId = LedgerEvent.Id(0, 1)
        )

      _ <- run(EitherT.right[TestError](jointLedger ? req))
    } yield (depositRefundTxSeq, req)
  }

  val _ = property("Joint Ledger Happy Path") = monadic(
    runner = runner,
    m = for {
      // Step 1: Set up the actor system and run the initialization tx, put the joint ledger in producing mode
      initTestEnv <- initializationScenario
      _ <- run(EitherT.right[TestError](initTestEnv.jointLedger ! StartBlock(1, Set.empty)))

      // Step 2: generate a deposit and observe that it appears in the dapp ledger correctly
      seqAndReq <- depositScenario(initTestEnv)
      (depositRefundTxSeq, depositReq) = seqAndReq
      s <- run(EitherT.right[TestError](initTestEnv.dappLedger ?: GetState))

      _ <- assertWith[ET](
        msg = s"We should only have 1 deposit in the state. We have ${s.deposits.length}",
        condition = s.deposits.length == 1
      )
      _ <- assertWith[ET](
        msg = "Correct deposit(s) in state",
        condition = s.deposits.head._2 == depositRefundTxSeq.depositTx.depositProduced
      )
      _ <- assertWith[ET](
        msg = "Correct treasury in state",
        condition = s.treasury == initTestEnv.initTx.initializationTx.treasuryProduced
      )

      // Step 3: Complete a block
      _ <- run(EitherT.right(initTestEnv.jointLedger ! testMCompleteBlockRegular(None)))
      jointLedgerState <- run(EitherT.right(initTestEnv.jointLedger ?: JointLedger.Requests.GetState))
      _ <- assertWith[ET](
        msg = "Finished block should be minor",
        condition = jointLedgerState match {
          case Done(block: Block.Minor) => true
          case _ => false
        }
      )

      // Step 4: Complete another block.
      _ <- run(EitherT.right[TestError](initTestEnv.jointLedger ! StartBlock(2, Set(depositReq.eventId))))
      _ <- run(EitherT.right(initTestEnv.jointLedger ! CompleteBlockRegular(None)))

      jointLedgerState <- run(EitherT.right(initTestEnv.jointLedger ?: JointLedger.Requests.GetState))
      majorBlock <- jointLedgerState match {
          case Done(block: Block.Major) => monadForPropM[ET].pure(block)
          case _ => PropertyM.fail_[ET, Block.Major]("finished block should be major")
        }

      kzgCommit <- run(EitherT.right(initTestEnv.virtualLedger ?: VirtualLedger.GetCurrentKzgCommitment))
      expectedUtxos = L2EventGenesis(depositRefundTxSeq.depositTx.depositProduced.virtualOutputs,
          TransactionHash.fromByteString(platform.blake2b_256(initTestEnv.config.tokenNames.headTokenName.bytes ++ 
            ByteString.fromBigIntBigEndian(BigInt(Full.unapply(majorBlock.header.blockVersion)._1))))).asUtxos
      
      _ <- assertWith[ET](
        msg = "KZG Commitment is correct",
        condition = kzgCommit == KzgCommitment.calculateCommitment(KzgCommitment.hashToScalar(expectedUtxos))
      )


    } yield true
  )
}

