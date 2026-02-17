package hydrozoa.multisig.ledger.virtual.commitment

import cats.data.EitherT
import cats.effect.IO
import hydrozoa.multisig.ledger.virtual.commitment.KzgCommitment.{KzgCommitment, asG1Element, asScalusScalar, hashToScalar, kzgCommitment}
import hydrozoa.rulebased.ledger.dapp.script.plutus.RuleBasedTreasuryValidator
import scalus.uplc.builtin.BLS12_381_G2_Element
import scalus.cardano.ledger.Utxos
import supranational.blst.P2

/** Membership check is required when withdrawing.
  */
object Membership {

    type KzgProof = KzgCommitment

    /** The straightforward approach to proofs:
      *   - Build the proof using the [[set]] and [[subset]] provided.
      *   - Verify the proof against the commitment provided.
      *
      * @param kzgCommitment
      *   the commitment to [[utxos]]
      * @param set
      *   the set of all utxos
      * @param subset
      *   the subset that we want to withdraw
      * @return
      *   the proof that [[subset]] is a subset of [[set]] indeed, which is no more no less than a
      *   new KZG commitment for ([[set]] \ [[subset]]). None if the check failed, which indicates
      *   that the [[kzgCommitment]] is wrong.
      */
    def mkMembershipProofValidated(
        kzgCommitment: KzgCommitment,
        set: Utxos,
        subset: Utxos
    ): IO[Either[MembershipCheckError, KzgProof]] = {
        import MembershipCheckError.*

        extension [A](io: IO[A])
            private def lift: EitherT[IO, MembershipCheckError, A] =
                EitherT(io.attempt.map(_.left.map(err => UnexpectedError(err.getMessage))))

        (for {
            // 1. Check the subset
            _ <- EitherT.cond[IO](subset.toSet.subsetOf(set.toSet), (), WrongSubset)
            rest = set -- subset.keys

            // 2. Check that the setup is big enough
            monomialG2 <- IO.delay(TrustedSetup.setup.g2Monomial).lift
            _ <- EitherT.cond[IO](monomialG2.sizeIs >= subset.size + 1, (), SubsetIsTooLarge)
            crsG2 = TrustedSetup.takeSrsG2(subset.size + 1).map(BLS12_381_G2_Element.apply)

            // 3. Build the proof
            proof <- IO { rest.kzgCommitment }.lift

            // 4. Validate the membership proof
            commitmentG1 = kzgCommitment.asG1Element
            proofG1 = proof.asG1Element
            subsetScalars = hashToScalar(subset).map(_.asScalusScalar)

            membershipCheck <- IO {
                RuleBasedTreasuryValidator.checkMembership(
                  setup = crsG2,
                  acc = commitmentG1,
                  subset = subsetScalars,
                  proof = proofG1
                )
            }.lift

            // 5. Return proof if check passes
            result <- EitherT.cond[IO](membershipCheck, proof, WrongSubset)
        } yield result).value
    }

    enum MembershipCheckError:
        case WrongSubset
        case SubsetIsTooLarge
        case UnexpectedError(err: String)

        def explain: String = this match {
            case MembershipCheckError.WrongSubset =>
                "The set of all utxos doesn't contain some elements from the subset"
            case MembershipCheckError.SubsetIsTooLarge =>
                "The subset is too big for the trusted setup size"
            case UnexpectedError(err) => s"Unexpected error occurred: $err"
        }
}
