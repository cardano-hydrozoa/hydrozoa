package hydrozoa.multisig.ledger

import cats.*
import cats.data.*
import cats.effect.*
import cats.effect.unsafe.implicits.*
import cats.syntax.all.*
import com.suprnation.actor.ActorRef.ActorRef
import com.suprnation.actor.{ActorSystem, test as _}
import hydrozoa.maxNonPlutusTxFee
import hydrozoa.multisig.ledger.DappLedger.Requests.{GetState, RegisterDeposit}
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.tx.InitializationTx.SpentUtxos
import hydrozoa.multisig.ledger.dapp.tx.{Tx, minInitTreasuryAda}
import hydrozoa.multisig.ledger.dapp.txseq.{DepositRefundTxSeq, InitializationTxSeq}
import hydrozoa.multisig.ledger.dapp.utxo.DepositUtxo
import hydrozoa.multisig.ledger.virtual.GenesisObligation
import hydrozoa.multisig.protocol.types.LedgerEvent
import org.scalacheck.*
import org.scalacheck.Prop.propBoolean
import org.scalacheck.rng.Seed
import scalus.builtin.ByteString
import scalus.cardano.address.ShelleyPaymentPart.Key
import scalus.cardano.ledger.*
import scalus.prelude.Option as SOption
import scalus.testing.kit.TestUtil
import test.Generators.Hydrozoa.*
import test.{TestPeer, genTestPeers, minPubkeyAda, nonSigningValidators, signTx, sumUtxoValues, testEvaluator, testNetwork, testProtocolParams, testTxBuilderEnvironment}

object DappLedgerTest extends Properties("DappLedger") {
    import org.scalacheck.PropertyM.*

    override def overrideParameters(p: Test.Parameters): Test.Parameters = {
        p
            .withMinSuccessfulTests(100)
            .withInitialSeed(Seed.fromBase64("W28rrQBwU4e2me7TydWPZDGl22_0duuU4iuVz5Y6QxN=").get)
    }

    type TestError = InitializationTxSeq.Builder.Error | DepositRefundTxSeq.Builder.Error |
        DappLedger.Errors.RegisterDepositError

    type ET[A] = EitherT[IO, TestError, A]

    // Accept "any" as the error
    def runner(mProp: ET[Prop]): Prop =
        Prop.secure(mProp.value.unsafeRunSync() match {
            case Left(e) =>
                s"Failed: $e" |: false
            case Right(p) => p
        })

    // FIXME(?): I used "Any" for the error type here, simply because I was too lazy to accumulate all of the
    // possible errors. Maybe it should be "Throwable"?

    case class InitializedTestEnvironment(
        hns: HeadMultisigScript,
        peers: NonEmptyList[TestPeer],
        actorSystem: ActorSystem[IO],
        initTx: InitializationTxSeq,
        config: Tx.Builder.Config,
        virtualLedger: ActorRef[IO, VirtualLedger.Request],
        dappLedger: ActorRef[IO, DappLedger.Requests.Request]
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
                  votingDuration = 100,
                  txTiming = ???,
                  initializedOn = ???
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
                  new DappLedger(initTx.initializationTx.treasuryProduced, config, virtualLedger) {}
                )
              )
            )

        } yield InitializedTestEnvironment(
          hns,
          peers,
          system,
          initTx,
          config,
          virtualLedger,
          dappLedger
        )

    val _ = property("DappLedger Register Deposit Happy Path") = monadic(
      runner = runner,
      m = for {
          initTestEnv <- initializationScenario
          peer = initTestEnv.peers.head

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
            config = initTestEnv.config,
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

          depositRefundTxSeq <- run(
            EitherT.fromEither[IO](depositRefundSeqBuilder.build).leftWiden[TestError]
          )

          signedTx = signTx(peer, depositRefundTxSeq.depositTx.tx)

          serializedDeposit = signedTx.toCbor
          req = RegisterDeposit(
            serializedDeposit = serializedDeposit,
            virtualOutputs = virtualOutputs,
            eventId = LedgerEvent.Id(0, 1)
          )

          _ <- run(EitherT(initTestEnv.dappLedger ?: req).leftWiden[TestError])
          s <- run(EitherT.right[TestError](initTestEnv.dappLedger ?: GetState))

          _ <- assertWith[ET](
            msg = s"We should only have 1 deposit in the state, but we have ${s.deposits.length}",
            condition = s.deposits.length == 1
          )
          _ <- assertWith[ET](
            msg = "Incorrect deposit(s) in state",
            condition = s.deposits.head._2 == depositRefundTxSeq.depositTx.depositProduced
          )
          _ <- assertWith[ET](
            msg = "Incorrect treasury in state",
            condition = s.treasury != initTestEnv.initTx.initializationTx.treasuryProduced
          )
      } yield true
    )
}
