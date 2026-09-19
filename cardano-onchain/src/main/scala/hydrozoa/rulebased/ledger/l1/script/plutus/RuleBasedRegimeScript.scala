package hydrozoa.rulebased.ledger.l1.script.plutus

import hydrozoa.lib.cardano.scalus.cardano.onchain.plutus.ByteStringExtension.take
import hydrozoa.lib.cardano.scalus.cardano.onchain.plutus.ValueExtension.*
import hydrozoa.rulebased.ledger.l1.script.plutus.RuleBasedRegimeValidator.RegimeRedeemer.Deinit
import hydrozoa.rulebased.ledger.l1.script.plutus.RuleBasedTreasuryValidator.{cip67BeaconTokenPrefix, cip67RegimeTokenPrefix}
import scalus.*
import scalus.cardano.address.ShelleyDelegationPart.Null
import scalus.cardano.address.{Network, ShelleyAddress, ShelleyPaymentPart}
import scalus.cardano.onchain.plutus.prelude.*
import scalus.cardano.onchain.plutus.prelude.Option.{None, Some}
import scalus.cardano.onchain.plutus.v3.{Validator, *}
import scalus.compiler.Compile
import scalus.uplc.PlutusV3
import scalus.uplc.builtin.*

/** Guards the rule-based regime utxo: its HRWT beacon and head-identity datum are readable by every
  * rule-based validator as a reference input, and the only way to spend it is the DeinitTx, which
  * burns the beacon along with the treasury's head tokens.
  *
  * The utxo sits here rather than at the head multisig address so that a sweep of that address
  * cannot consume it: losing the regime utxo strands the rule-based treasury, which needs it as a
  * reference input to resolve and to evacuate.
  */
@Compile
object RuleBasedRegimeValidator extends Validator {

    /** Script redeemer. The regime utxo has exactly one way out. */
    enum RegimeRedeemer:
        case Deinit

    given FromData[RegimeRedeemer] = FromData.derived

    given ToData[RegimeRedeemer] = ToData.derived

    private inline val OwnInputNotFound =
        "Impossible happened: own input was not found"
    private inline val RegimeTokenMissing =
        "Regime utxo must hold exactly one HRWT and no other token"
    private inline val HeadTokensNotBurned =
        "Deinit must burn tokens under the head multisig policy"
    private inline val RegimeTokenNotBurned =
        "Deinit must burn the HRWT held by the regime utxo"
    private inline val TreasuryTokenNotBurned =
        "Deinit must burn exactly one treasury beacon token"

    // Entry point
    override inline def spend(
        datum: Option[Data],
        redeemer: Data,
        tx: TxInfo,
        ownRef: TxOutRef
    ): Unit =

        log("RegimeValidator")

        redeemer.to[RegimeRedeemer] match
            case Deinit =>
                log("Deinit")

                val ownInput = tx.inputs
                    .find(_.outRef === ownRef)
                    .getOrFail(OwnInputNotFound)
                    .resolved

                // The HRWT is the regime utxo's only non-ada asset, so its policy is the head
                // multisig policy — the one authentication this validator needs. Minting under it
                // takes the head multisig's unanimous witness.
                val (headMp, regimeTokenName, regimeTokenAmount) = ownInput.value.onlyNonAdaAsset
                require(
                  regimeTokenName.take(4) == cip67RegimeTokenPrefix,
                  RegimeTokenMissing
                )

                val headTokensBurned = (-tx.mint).toSortedMap
                    .get(headMp)
                    .getOrFail(HeadTokensNotBurned)

                // Every token this utxo carries must be burned, so nothing survives the spend to
                // be re-locked or swept elsewhere.
                val regimeTokenBurned = headTokensBurned.get(regimeTokenName) match
                    case Some(burned) => burned == regimeTokenAmount
                    case None         => false
                require(regimeTokenBurned, RegimeTokenNotBurned)

                // The treasury beacon must go in the same tx. This check is what ties the regime
                // utxo's lifetime to the treasury's: the beacon leaves the treasury only through
                // its Deinit branch, which requires the treasury to be resolved and fully
                // evacuated. An HRWT-only check would let the regime utxo go while the treasury
                // still needs it to resolve or evacuate.
                headTokensBurned.toList.filter((tokenName, _) =>
                    tokenName.take(4) == cip67BeaconTokenPrefix
                ) match
                    case List.Cons(tokenNameAndAmount, none) =>
                        require(
                          none.isEmpty && tokenNameAndAmount._2 == BigInt(1),
                          TreasuryTokenNotBurned
                        )
                    case _ => fail(TreasuryTokenNotBurned)
}

object RuleBasedRegimeScript {
    // Compile the validator using PlutusV3.compile
    given scalus.compiler.Options = scalus.compiler.Options.default

    val compiledPlutusV3Program: PlutusV3[Data => Unit] =
        PlutusV3.compile(RuleBasedRegimeValidator.validate)

    private val compiledScriptHash: ScriptHash = compiledPlutusV3Program.script.scriptHash

    def address(n: Network): ShelleyAddress =
        ShelleyAddress(
          network = n,
          payment = ShelleyPaymentPart.Script(
            scalus.cardano.ledger.ScriptHash.fromArray(this.compiledScriptHash.bytes)
          ),
          delegation = Null
        )
}
