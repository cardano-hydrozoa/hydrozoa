package test

import cats.*
import cats.data.*
import cats.effect.*
import cats.effect.unsafe.IORuntime
import cats.syntax.all.*
import com.suprnation.actor.ActorRef.ActorRef
import com.suprnation.actor.ActorSystem
import hydrozoa.config.EquityShares
import hydrozoa.maxNonPlutusTxFee
import hydrozoa.multisig.ledger.*
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.tx.InitializationTx.SpentUtxos
import hydrozoa.multisig.ledger.dapp.tx.{Tx, minInitTreasuryAda}
import hydrozoa.multisig.ledger.dapp.txseq.{DepositRefundTxSeq, InitializationTxSeq}
import hydrozoa.multisig.ledger.virtual.commitment.KzgCommitment
import hydrozoa.rulebased.ledger.dapp.tx.genEquityShares
import org.scalacheck.Prop.propBoolean
import org.scalacheck.PropertyM.{monadForPropM, monadic}
import org.scalacheck.{Gen, Prop, PropertyM}
import scala.concurrent.duration.{FiniteDuration, SECONDS}
import scalus.builtin.ByteString
import scalus.cardano.address.ShelleyPaymentPart.Key
import scalus.cardano.ledger.{AddrKeyHash, Coin, Utxo}
import scalus.testing.kit.TestUtil
import test.Generators.Hydrozoa.genAdaOnlyPubKeyUtxo

/** The "environment" that is contained in the ReaderT of the TestM
  * @param peers
  * @param actorSystem
  * @param initTx
  * @param config
  * @param virtualLedger
  * @param dappLedger
  * @param jointLedger
  */
// TODO (Peter,  2025-12-23): This is one particular place where the "row types" extension can be extremely useful.
// In particular, there may be certain tests where we don't _actually_ want to instantiate the entire TestR environment.
// So instead of hard-coding TestR into TestM, we could instead:
// - Define TestR as below, and have it extend "HasPeers", "HasActorSystem", etc.
// - Parameterize TestM's reader environment such that it can be modified to take a `R : HasPeers : HasActorSystem`
//   if only those fields are actually necessary.
// This helps a lot with mocks. I'm not going to worry about it for now, because its both pretty noisy to do in scala
// (compared to purescript using row types or haskell using labelled optics), and we probably won't need it for an MVP.
case class TestR(
    peers: NonEmptyList[TestPeer],
    actorSystem: ActorSystem[IO],
    initTx: InitializationTxSeq, // Move to HeadConfig
    config: Tx.Builder.Config, // Move to HeadConfig
    jointLedger: ActorRef[IO, JointLedger.Requests.Request],
)

type TestError =
    InitializationTxSeq.Builder.Error | DepositRefundTxSeq.Builder.Error |
        DappLedgerM.Error.RegisterDepositError

private type ET[A] = EitherT[IO, TestError, A]
private type PT[A] = PropertyM[ET, A]
private type RT[A] = ReaderT[PT, TestR, A]

/** Describes a computation that:
  *   - Has access to some [[TestR]] environment
  *   - Accepts continuations (within [[PropertyM]])
  *   - Can throw errors of type [[TestError]]
  *   - Can perform [[IO]]
  *   - Can generate values (via the [[Gen]] within [[PropertyM]]
  *   - Returns values of type A
  * @param unTestM
  * @tparam A
  */
case class TestM[A](unTestM: RT[A]) {
    def map[B](f: A => B): TestM[B] = TestM(this.unTestM.map(f))
    def flatMap[B](f: A => TestM[B]): TestM[B] = TestM(this.unTestM.flatMap(a => f(a).unTestM))
}

object TestM {

    /** Get the instantiated TestR test environment
      */
    val ask: TestM[TestR] = TestM(Kleisli.ask)
    // TODO: Right now, this generates everything. In the future, we can provide arguments like
    // `peers :: Option[NonEmptyList[TestPeer]]` such that we set the peers exactly to the option,
    // or generate otherwise. This goes along with the comment on [[run]] for passing initializers directly to run
    private val defaultInitializer: PropertyM[ET, TestR] = {
        for {
            peers <- PropertyM.pick[ET, NonEmptyList[TestPeer]](genTestPeers.label("Test Peers"))

            // We make sure that the seed utxo has at least enough for the treasury and multisig witness UTxO, plus
            // a max non-plutus fee
            seedUtxo <- PropertyM.pick[ET, Utxo](
              genAdaOnlyPubKeyUtxo(
                peers.head,
                minimumCoin = minInitTreasuryAda
                    + Coin(maxNonPlutusTxFee(testProtocolParams).value * 2)
              ).map(x => Utxo(x._1, x._2)).label("Initialization: seed utxo")
            )

            otherSpentUtxos <- PropertyM.pick[ET, List[Utxo]](
              Gen
                  .listOf(genAdaOnlyPubKeyUtxo(peers.head))
                  .map(_.map(x => Utxo(x._1, x._2)))
                  .label("Initialization: other spent utxos")
            )

            spentUtxos = NonEmptyList(seedUtxo, otherSpentUtxos)

            // Initial deposit must be at least enough for the minAda of the treasury, and no more than the
            // sum of the seed utxos, while leaving enough left for the estimated fee and the minAda of the change
            // output
            initialDeposit <- PropertyM.pick[ET, Coin](
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

            system <- PropertyM.run(
              EitherT.right[TestError](ActorSystem[IO]("DappLedger").allocated.map(_._1))
            )
            initTx <- PropertyM.run(
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

            equityShares <- PropertyM.pick[ET, EquityShares](
              genEquityShares(peers).label("Equity shares")
            )

            jointLedger <- PropertyM.run(
              EitherT.right[TestError](
                system.actorOf(
                  JointLedger(
                    peerLiaisons = Seq.empty,
                    tallyFeeAllowance = Coin.ada(2),
                    initialBlockTime = FiniteDuration(0, SECONDS), // FIXME: Generate
                    initialBlockKzg = KzgCommitment.empty,
                    equityShares = equityShares,
                    multisigRegimeUtxo = config.multisigRegimeUtxo,
                    votingDuration = 0,
                    treasuryTokenName = config.tokenNames.headTokenName,
                    initialTreasury = initTx.initializationTx.treasuryProduced,
                    config = config
                  )
                )
              )
            )

        } yield TestR(
          peers,
          system,
          initTx,
          config,
          jointLedger
        )
    }

    def asks[A](f: TestR => A): TestM[A] =
        for {
            env <- ask
        } yield f(env)

    def pick[A](gen: Gen[A]): TestM[A] = TestM(Kleisli.liftF(PropertyM.pick(gen)))

    def pure[A](a: A): TestM[A] = TestM(Kleisli.pure(a))

    def fail[A](msg: String): TestM[A] = TestM(Kleisli.liftF(PropertyM.fail_(msg)))

    def assertWith(condition: Boolean, msg: String): TestM[Unit] =
        TestM(Kleisli.liftF(PropertyM.assertWith(condition, msg)))

    /** Given a computation of type [[TestM]] that returns a value that can be implicitly turned
      * into a [[Prop]], run the computation.
      * @param testM
      *   The computation to run
      * @param initializer
      *   the computation that generates and sets up the [[TestR]] environment passed to [[testM]].
      *   Defaults to a (sensibly) randomly generated environment.
      * @param toProp
      *   The implicit function that transforms the result of the computation into a [[Prop]]
      * @param ioRuntime
      *   The implicit IO runtime in which [[IO]] effects can be executed
      * @tparam A
      * @return
      */
    def run[A](testM: TestM[A], initializer: PT[TestR] = defaultInitializer)(using
        toProp: A => Prop,
        ioRuntime: IORuntime
    ): Prop = {

        // The runner tries to eliminate the EitherT[IO, TestError, Prop] to get an Prop.
        // If it gets a Left[TestError], it means no property could be extracted, so we consider it a test failure and
        // report the error.
        // If we can generate a prop, we just return it.
        def runner(mProp: ET[Prop]): Prop =
            Prop.secure(mProp.value.unsafeRunSync() match {
                case Left(e) =>
                    s"Failed: $e" |: false
                case Right(p) => p
            })

        monadic(
          runner = runner,
          m =
              // This runs the initialization within the `PropertyM` first, in order to give the computation in `TestM`
              // access to the fully-initialized environment
              for {
                  env <- initializer
                  res <- testM.unTestM.run(env)
              } yield res
        )
    }

    // ===================================
    // Lifts
    // ===================================
    def liftR[A](io: IO[A]): TestM[A] = {
        TestM(
          unTestM = Kleisli.liftF(PropertyM.run(EitherT.right(io)))
        )
    }

    def liftL[A](io: IO[TestError]): TestM[A] =
        TestM(Kleisli.liftF(PropertyM.run(EitherT.left(io))))

    def lift[A](e: Either[TestError, A]): TestM[A] =
        TestM(Kleisli.liftF(PropertyM.run(EitherT.fromEither(e))))

}
