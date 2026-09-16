package hydrozoa.multisig.consensus

import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.multisig.ledger.l1.tx.{SettlementTx, genSettlementTxSeqBuilder}
import org.scalacheck.Gen
import org.scalacheck.rng.Seed
import org.scalatest.funsuite.AnyFunSuite
import test.TestPeersSpec

/** Probe: does a generated, multisigned settlement satisfy the validator set
  * [[JoinOfferVerifier.historicalValidators]] runs?
  *
  * This is the assumption the settlement half of the verifier rests on, and it has not been tested.
  * If a real settlement does not pass, the verifier would refuse every honest offer — a failure
  * that would look exactly like a security check working.
  */
class JoinOfferVerifierSettlementTest extends AnyFunSuite:

    private val env: MultiNodeConfig =
        MultiNodeConfig
            .generate(TestPeersSpec.default)()
            .pureApply(Gen.Parameters.default, Seed(0L))

    private given JoinOfferVerifier.Config = env.headConfig

    /** A settlement as the builder produces it, with every head peer's witness attached. */
    private val signedSettlement: SettlementTx =
        val builder = genSettlementTxSeqBuilder(env.headConfig)()
            .pureApply(Gen.Parameters.default, Seed(1L))
        val txSeq = builder.result match {
            case Left(e)  => throw RuntimeException(s"settlement build failed: $e")
            case Right(s) => s
        }
        val unsigned = txSeq.settlementTx
        unsigned.txLens.replace(env.multisignTx(unsigned.tx))(unsigned)

    test("a real multisigned settlement passes the historical validator set") {
        val result = JoinOfferVerifier.checkSettlementValid(signedSettlement)
        assert(result.isRight, s"honest settlement was refused: $result")
    }

    test("an unsigned settlement is refused — the check is not vacuous") {
        // The same settlement without any head-peer witness. If this passed, the validator set
        // would be accepting anything and the settlement check would be decoration.
        val builder = genSettlementTxSeqBuilder(env.headConfig)()
            .pureApply(Gen.Parameters.default, Seed(1L))
        val unsigned = builder.result match {
            case Left(e)  => throw RuntimeException(s"settlement build failed: $e")
            case Right(s) => s.settlementTx
        }
        val result = JoinOfferVerifier.checkSettlementValid(unsigned)
        assert(
          result.left.exists(_.isInstanceOf[JoinRefusal.SettlementInvalid]),
          s"an unsigned settlement was accepted: $result"
        )
    }
