package hydrozoa.multisig.ledger.commitment

import hydrozoa.multisig.ledger.joint.EvacuationMap
import hydrozoa.rulebased.ledger.l1.script.plutus.RuleBasedTreasuryValidator
import scala.util.Try
import scalus.uplc.builtin.bls12_381.G2Element
import supranational.blst.P2

import KzgCommitment.{KzgCommitment, asG1Element, asScalusScalar}

/** Membership check is required when withdrawing.
  */
object Membership {

    type KzgProof = KzgCommitment

    /** The straightforward approach to proofs:
      *   - Build the proof using the [[set]] and [[subset]] provided.
      *   - Verify the proof against the commitment provided.
      *
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
        set: EvacuationMap,
        subset: EvacuationMap
    ): Either[MembershipCheckError, KzgProof] = Try {
        import MembershipCheckError.*

        for {
            // 1. Check the subset
            _ <- Either.cond(subset.subsetOf(set), (), WrongSubset)
            rest = set.removedAll(subset.evacuationMap.keySet)

            // 2. Check that the setup is big enough
            monomialG2 = TrustedSetup.setup.g2Monomial
            _ <- Either.cond(monomialG2.sizeIs >= subset.size + 1, (), SubsetIsTooLarge)
            crsG2 = TrustedSetup.takeSrsG2(subset.size + 1).map(G2Element.apply)

            // 3. Build the proof
            proof = rest.kzgCommitment

            // 4. Validate the membership proof
            commitmentG1 = set.kzgCommitment.asG1Element
            proofG1 = proof.asG1Element
            subsetScalars = subset.scalars.map(_.asScalusScalar)

            membershipCheck =
                RuleBasedTreasuryValidator.checkMembership(
                  setup = crsG2,
                  acc = commitmentG1,
                  subset = subsetScalars,
                  proof = proofG1
                )

            // 5. Return proof if check passes
            result <- Either.cond(membershipCheck, proof, WrongSubset)
        } yield result
    }.toEither.left.map(e => MembershipCheckError.UnexpectedError(e.getMessage)).flatten

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
