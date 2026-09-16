package hydrozoa.multisig.consensus

import cats.effect.unsafe.implicits.global
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.joint.EvacuationMap
import hydrozoa.multisig.ledger.l1.tx.{SettlementTx, genSettlementTxSeqBuilder}
import hydrozoa.multisig.ledger.l1.utxo.MultisigTreasuryUtxo
import hydrozoa.multisig.ledger.l2.{L2Ledger, L2StateHash}
import org.scalacheck.Gen
import org.scalacheck.rng.Seed
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.{AssetName, Hash32}
import scalus.uplc.builtin.ByteString
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

    // ---- the checks that bind a settlement to THIS head, and the state to the certificate -------

    private val treasury = signedSettlement.treasuryProduced

    /** The digests a coil's own ledger would report after adopting a state that genuinely matches
      * this settlement's certificate.
      */
    private val matchingDigests: L2Ledger.Digests =
        L2Ledger.Digests(
          evacuationMapHash = EvacuationMap.empty.digest,
          evacuationMapKzg = treasury.kzgCommitment,
          l2StateHash = L2StateHash(treasury.datum.l2StateHash),
          l2ParamsHash = env.headConfig.l2ParamsHash
        )

    /** Replace the produced treasury, whichever settlement shape this fixture turned out to be. */
    private def withTreasury(s: SettlementTx, t: MultisigTreasuryUtxo): SettlementTx = s match {
        case x: SettlementTx.NoPayouts             => x.copy(treasuryProduced = t)
        case x: SettlementTx.WithOnlyDirectPayouts => x.copy(treasuryProduced = t)
        case x: SettlementTx.WithRollouts          => x.copy(treasuryProduced = t)
    }

    private def verify(
        settlement: SettlementTx = signedSettlement,
        digests: L2Ledger.Digests = matchingDigests
    ): Either[JoinRefusal, Unit] =
        JoinOfferVerifier.verify(settlement, sec = None, adopted = digests).unsafeRunSync()

    test("a settlement and matching state are accepted end to end") {
        assert(verify() == Right(()), s"an honest offer was refused: ${verify()}")
    }

    test("a treasury at another head's address is refused") {
        val elsewhere =
            withTreasury(
              signedSettlement,
              treasury.copy(address = env.addressOf(HeadPeerNumber.zero))
            )
        assert(
          verify(settlement = elsewhere).left.exists(
            _.isInstanceOf[JoinRefusal.WrongTreasuryAddress]
          )
        )
    }

    test("a treasury carrying another head's beacon token is refused") {
        // Right roster, right address, wrong head instance — the case the address check alone
        // cannot see.
        val otherHead = withTreasury(
          signedSettlement,
          treasury.copy(treasuryTokenName = AssetName(ByteString.fromString("nothead")))
        )
        assert(
          verify(settlement = otherHead).left.exists(_.isInstanceOf[JoinRefusal.WrongHeadId])
        )
    }

    test("a state digesting to something the certificate did not commit to is refused") {
        val wrongState = matchingDigests.copy(
          l2StateHash = L2StateHash(ByteString.fromArray(Array.fill[Byte](32)(0x99.toByte)))
        )
        assert(
          verify(digests = wrongState).left.exists(_.isInstanceOf[JoinRefusal.L2StateMismatch])
        )
    }

    test("a state whose evacuation map is not the certified one is refused") {
        val wrongMap = matchingDigests.copy(
          evacuationMapKzg = ByteString.fromArray(Array.fill[Byte](48)(0x77.toByte))
        )
        assert(
          verify(digests = wrongMap).left.exists(
            _.isInstanceOf[JoinRefusal.EvacuationMapMismatch]
          )
        )
    }

    test("a state from a different ledger build is refused") {
        val wrongParams = matchingDigests.copy(
          l2ParamsHash = Hash32.fromByteString(ByteString.fromArray(Array.fill[Byte](32)(0x11)))
        )
        assert(
          verify(digests = wrongParams).left.exists(_.isInstanceOf[JoinRefusal.L2ParamsMismatch])
        )
    }
