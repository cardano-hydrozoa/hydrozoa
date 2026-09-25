package hydrozoa.multisig.consensus

import cats.effect.unsafe.implicits.global
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.multisig.ledger.block.{BlockNumber, BlockVersion}
import hydrozoa.multisig.ledger.joint.EvacuationMap
import hydrozoa.multisig.ledger.l2.L2StateHash
import hydrozoa.multisig.ledger.stack.StandaloneEvacuationCommitment
import hydrozoa.rulebased.ledger.l1.state.StandaloneEvacuationCommitmentOnchain
import org.scalacheck.Gen
import org.scalacheck.rng.Seed
import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.ByteString

/** The SEC half of [[JoinOfferVerifier]]: the signatures a coil peer requires before it will adopt
  * a start point sitting in a minor partition.
  *
  * **Why the SEC gets its own suite.** A settlement is a transaction, so scalus's
  * `NativeScriptsValidator` enforces its `AllOf(head) + MOf(coilQuorum, coils)` script for us. An
  * SEC is not — it is a bare set of header signatures the dispute validator reads — so every rule
  * it must satisfy is enforced in `verifySecSignatures` or nowhere at all.
  *
  * ⚠️ The fixtures sign with the **last** `coilQuorum` coils, a non-prefix subset (see
  * `MultiNodeConfig.multisignHeaderSparse`). Prefix-signing fixtures would pass an implementation
  * that packed coil signatures densely or read them at the wrong offsets; these do not.
  */
class JoinOfferVerifierSecTest extends AnyFunSuite:

    private val nCoils = 5
    private val quorum = 3

    private val env: MultiNodeConfig =
        MultiNodeConfig
            .generateWithCoil(nCoil = nCoils, quorum = quorum)
            .pureApply(Gen.Parameters.default, Seed(0L))

    private given JoinOfferVerifier.Config = env.headConfig

    private val testL2StateHash = ByteString.fromArray(Array.fill[Byte](32)(0x5c.toByte))

    private val evacuationMap = EvacuationMap.empty

    private val blockHeader = StandaloneEvacuationCommitmentOnchain(
      headId = env.headConfig.headTokenNames.treasuryTokenName.bytes,
      versionMajor = 1,
      versionMinor = 1,
      commitment = evacuationMap.kzgCommitment,
      l2StateHash = testL2StateHash
    )

    /** A hard-confirmed SEC carrying whatever signature list a test wants to try. */
    private def secWith(
        signatures: List[Option[StandaloneEvacuationCommitment.Signature]]
    ): StandaloneEvacuationCommitment.MultiSigned =
        StandaloneEvacuationCommitment.MultiSigned(
          commitment = StandaloneEvacuationCommitment(
            blockNum = BlockNumber(1),
            blockVersion = BlockVersion.Full(1, 1),
            kzgCommitment = evacuationMap.kzgCommitment,
            l2StateHash = L2StateHash(testL2StateHash),
            header = StandaloneEvacuationCommitmentOnchain(blockHeader)
          ),
          signatures = signatures
        )

    /** What a real hard-confirmation produces: every head peer, then the last `quorum` coils. */
    private val goodSignatures: List[Option[StandaloneEvacuationCommitment.Signature]] =
        env.multisignHeaderSparse(blockHeader)

    private val nHead: Int = env.headConfig.headPeerVKeys.toList.size

    /** The index of the first coil slot that actually carries a signature. */
    private val aSigningCoil: Int =
        goodSignatures.zipWithIndex.collectFirst { case (Some(_), i) if i >= nHead => i }.get

    private def check(
        signatures: List[Option[StandaloneEvacuationCommitment.Signature]]
    ): Either[JoinRefusal, Unit] =
        JoinOfferVerifier.verifySecSignatures(Some(secWith(signatures))).unsafeRunSync()

    test("a properly hard-confirmed SEC is accepted") {
        assert(check(goodSignatures) == Right(()))
    }

    test("no SEC at all is accepted — a major start point carries none") {
        assert(JoinOfferVerifier.verifySecSignatures(None).unsafeRunSync() == Right(()))
    }

    test("a missing head-peer signature is refused") {
        assert(
          check(goodSignatures.updated(0, None)) ==
              Left(JoinRefusal.SecSignatureInvalid(0, missing = true))
        )
    }

    test("a head-peer signature made by the wrong key is refused") {
        // Present and well-formed, but peer 1's signature sitting in peer 0's slot.
        assert(
          check(goodSignatures.updated(0, goodSignatures(1))) ==
              Left(JoinRefusal.SecSignatureInvalid(0, missing = false))
        )
    }

    test("one coil signature short of quorum is refused") {
        val short = goodSignatures.updated(aSigningCoil, None)
        assert(check(short) == Left(JoinRefusal.SecCoilQuorumNotMet(quorum - 1, quorum)))
    }

    test("a coil slot that is filled but invalid does not count toward quorum") {
        // A head peer's signature dropped into a coil slot: the slot is occupied, so an
        // implementation that counted presence rather than validity would reach quorum here.
        val poisoned = goodSignatures.updated(aSigningCoil, goodSignatures.head)
        assert(check(poisoned) == Left(JoinRefusal.SecCoilQuorumNotMet(quorum - 1, quorum)))
    }

    test("coil signatures packed densely at the front are refused") {
        // The bug `multisignHeaderSparse` exists to guard. Every signature here is individually
        // valid and the count is exactly `quorum` — but each sits at a coil index whose vkey did
        // not make it, so none verifies and the quorum is zero.
        val coilSigs = goodSignatures.drop(nHead).flatten
        val packed = goodSignatures.take(nHead) ++
            coilSigs.map(Some(_)) ++
            List.fill(nCoils - coilSigs.size)(None)
        assert(check(packed) == Left(JoinRefusal.SecCoilQuorumNotMet(0, quorum)))
    }
