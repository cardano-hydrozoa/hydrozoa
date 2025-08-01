package hydrozoa.l2.ledger

import hydrozoa.l2.ledger
import scalus.cardano.address.ShelleyDelegationPart.Null
import scalus.cardano.address.{Address, ShelleyAddress}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.RedeemerTag.Spend
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.rules.UtxoEnv

//////////////////////////
// L2 Conformance: ensuring that L2 transactions and withdrawals (represented by L1 `Transaction`s) and genesis events
// conform to Hydrozoa's L2 restrictions (no collateral, no certs, etc.)

trait L2ConformanceValidator[L1]:
    // Notes (Peter, 2025-07-21):
    // - We're using string errors for now. We should change this.
    // - We're using Either[String,Unit] instead of Option[String]
    //   because 1.) Either will terminate on the first left (signifying failure),
    //   while Option will return on the first `None` (signifying success; isomorphic to `Right(())`),
    //   making `for`/`yield` easier with Either, and 2.) this is what the upstream uses.
    def l2Validate(l1: L1): Either[String, Unit]

object L2ConformanceValidator extends STSL2.Validator {
    override def validate(context: Context, state: State, event: Event): Result = event match {
        case L2EventTransaction(tx) => given_L2ConformanceValidator_Transaction.l2Validate(tx)
        case L2EventWithdrawal(tx)  => given_L2ConformanceValidator_Transaction.l2Validate(tx)
        case L2EventGenesis(resolvedSeq) =>
            mapLeft(_.toString)(resolvedSeq.foldLeft[Either[Error, Unit]](Right(()))((acc, resolved) =>
                if acc.isLeft then acc else given_L2ConformanceValidator_TransactionOutput.l2Validate(resolved._2)
            ))
    }
}

///////////////
// Helpers

/** Passes validation if the Option is None, or if the underlying value of the Some validates. */
def validateIfPresent[T](
    value: Option[T]
)(using v: L2ConformanceValidator[T]): Either[String, Unit] =
    value match {
        case Some(s) => v.l2Validate(s)
        case None    => Right(())
    }

private def validateEquals[T](msg: String)(actual: T)(expected: T): Either[String, Unit] =
    if actual == expected
    then Right(())
    else Left(s"[L2Conformance for ${msg}]: actual: ${actual}; expected: ${expected}")

///////////////
// Givens

given L2ConformanceValidator[Address] with
    /** L2 address must be Shelley addresses without delegation parts. */
    def l2Validate(addr: Address): Either[String, Unit] = addr match {
        case shelley: ShelleyAddress =>
            if shelley.delegation != Null then Left("Address has a delegation, but shouldn't")
            else Right(())
        case _ => Left("Address is not shelley")
    }

given L2ConformanceValidator[DatumOption] with
    /** Only inline datums are allowed */
    def l2Validate(dat: DatumOption): Either[String, Unit] = dat match {
        case (i: Inline) => Right(())
        case _           => Left("Datum not inline")
    }

given L2ConformanceValidator[TransactionOutput] with

    /** Differs from the L1 Transaction Output in that: \- Only babbage-style outputs are allowed \-
      * L2 transaction outputs can only contain Ada \- Datums, if present, must be inline \- Only
      * native scripts or v3 plutus scripts allowed in the script ref
      */
    def l2Validate(l1: TransactionOutput): Either[String, Unit] = {
        for
            _ <- l1 match {
                case _ : Babbage => Right(())
                case _ => Left(s"Transaction output is not a Babbage output. Output is: ${l1}")
            }
            _ <- validateIfPresent(l1.asInstanceOf[Babbage].datumOption)
            _ <- validateIfPresent(l1.asInstanceOf[Babbage].scriptRef)
        yield ()
    }

given L2ConformanceValidator[ScriptRef] with
    /** Only Native and PlutusV3 scripts are allowed */
    def l2Validate(l1: ScriptRef): Either[String, Unit] = l1.script match {
        case ns: Script.Native    => Right(())
        case pv3: Script.PlutusV3 => Right(())
        case _ => Left("Script ref contains a script that is not Native or PlutusV2")
    }

given L2ConformanceValidator[RedeemerTag] with
    /** Only the "Spend" redeemer tag is allowed */
    def l2Validate(l1: RedeemerTag): Either[String, Unit] =
        validateEquals("Redeemer Tag")(l1)(Spend)

given L2ConformanceValidator[Redeemer] with
    /** Redeemer tag must be spending */
    def l2Validate(l1: Redeemer): Either[String, Unit] =
        given_L2ConformanceValidator_RedeemerTag.l2Validate(l1.tag)

given L2ConformanceValidator[Redeemers] with
    def l2Validate(l1: Redeemers): Either[String, Unit] =
        // N.B.: I would have liked to use `traverse`, but I couldn't quite figure out how
        l1.toSeq.foldLeft[Either[String, Unit]](Right(()))((acc, redeemer) =>
            if acc.isLeft then acc else given_L2ConformanceValidator_Redeemer.l2Validate(redeemer)
        )

given L2ConformanceValidator[UtxoEnv] with
    /** L2 UtxoEnv has an empty CertState */
    def l2Validate(l1: UtxoEnv): Either[String, Unit] =
        if l1.certState != CertState.empty then Left("CertState not empty") else Right(())

given L2ConformanceValidator[Transaction] with
    def l2Validate(l1: Transaction): Either[String, Unit] =
        for
            _ <- given_L2ConformanceValidator_TransactionBody.l2Validate(l1.body.value)
            _ <- given_L2ConformanceValidator_TransactionWitnessSet.l2Validate(l1.witnessSet)
        yield Right(())

given L2ConformanceValidator[TransactionBody] with
    /** Differs from the L1 Tx Body as follows: \- The following omissions from the current spec
      * (commit e2ef186, 2025-07-08 version): \- No certificates, withdrawals, mints,
      * voting_procedures, proposal_procedures, current_treasury_value, or treasury_donation \-
      * Omitting fields related to fees and collateral (see private discussion at
      * https://discord.com/channels/@me/1387084765173121175/1389956276208926852; rationale being
      * that someone can quit consensus if scripts keep failing)
      */
    def l2Validate(
        l1: TransactionBody
    ): Either[String, Unit] =
        for
            // Validate prohibited fields from L1 transaction
            _ <- validateEquals("Collateral Inputs")(l1.collateralInputs)(Set.empty)
            _ <- validateEquals("Collateral Return Output")(l1.collateralReturnOutput)(None)
            _ <- validateEquals("Total Collateral")(l1.totalCollateral)(None)
            _ <- validateEquals("Certificates")(l1.certificates)(TaggedSet.empty)
            _ <- validateEquals("Withdrawals")(l1.withdrawals)(None)
            _ <- validateEquals("Fee")(l1.fee)(Coin(0L))
            _ <- validateEquals("Mint")(l1.mint)(None)
            _ <- validateEquals("Voting Procedures")(l1.votingProcedures)(None)
            _ <- validateEquals("Proposal Procedures")(l1.proposalProcedures)(Set.empty)
            _ <- validateEquals("Current Treasury Value")(l1.currentTreasuryValue)(None)
            _ <- validateEquals("Donation")(l1.donation)(None)
            // Validate nested fields from L1 transaction type
            // N.B.: I would have liked to use `traverse`, but I couldn't quite figure out how
            _ <- l1.outputs.foldLeft[Either[String, Unit]](Right(()))((acc, sto) => {
                if acc.isLeft then acc else given_L2ConformanceValidator_TransactionOutput.l2Validate(sto.value)
            })
        yield ()

given L2ConformanceValidator[TransactionWitnessSet] with
    /** Bootstrap witnesses, and plutus scripts < V3 are not allowed */
    def l2Validate(
        l1: TransactionWitnessSet
    ): Either[String, Unit] =
        for
            // Validate empty fields
            _ <- validateEquals("Bootstrap Witnesses")(l1.bootstrapWitnesses)(Set.empty)
            _ <- validateEquals("Plutus V1 Scripts")(l1.plutusV1Scripts)(Set.empty)
            _ <- validateEquals("Plutus V2 Scripts")(l1.plutusV2Scripts)(Set.empty)
            // Validate present fields
            _ <- validateIfPresent(l1.redeemers.map(_.value))
        yield ()
