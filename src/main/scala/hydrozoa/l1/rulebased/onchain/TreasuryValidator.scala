package hydrozoa.l1.rulebased.onchain

import hydrozoa.l1.multisig.state.L2ConsensusParamsH32
import hydrozoa.l1.rulebased.onchain.TreasuryValidator.TreasuryRedeemer.{Deinit, Resolve, Withdraw}
import scalus.Compiler.compile
import scalus.builtin.{ByteString, Data, FromData, ToData}
import scalus.ledger.api.v1.{CurrencySymbol, PosixTime, TokenName}
import scalus.ledger.api.v3.{TxInfo, TxOutRef}
import scalus.prelude.{Option, Validator, orFail}
import scalus.uplc.Program
import scalus.{Compile, Ignore, plutusV3, toUplc, showHighlighted}

@Compile
object TreasuryValidator extends Validator:

    // EdDSA / ed25519 Cardano verification key
    private type VerificationKey = ByteString

    // The result of `bls12_381_G2_compress` function
    private type BLSProof = ByteString

    // Datum
    enum TreasuryDatum:
        case Resolved(resolvedDatum: ResolvedDatum)
        case Unresolved(unresolvedDatum: UnresolvedDatum)

    case class ResolvedDatum(
        headMp: CurrencySymbol,
        utxosActive: BLSProof, // TODO: Accumulator commitment
        version: (BigInt, BigInt),
        params: L2ConsensusParamsH32
    )

    case class UnresolvedDatum(
        headMp: CurrencySymbol,
        disputeId: TokenName,
        peers: List[VerificationKey],
        peersN: BigInt,
        deadlineVoting: PosixTime,
        versionMajor: BigInt,
        params: L2ConsensusParamsH32
    )

    // Redeemer
    enum TreasuryRedeemer:
        case Resolve
        case Withdraw(withdrawRedeemer: WithdrawRedeemer)
        case Deinit

    given FromData[TreasuryRedeemer] = FromData.derived
    given ToData[TreasuryRedeemer] = ToData.derived

    case class WithdrawRedeemer(
        // I believe we can have only the proof here:
        // - outputs are in the transaction, and all except the treasury output are withdrawals
        // - the accumulator is based on hashes, not utxo ids, so we don't need `key` either
        // - I think we have to use the same network id, then we can hash directly
        proof: BLSProof
    )

    given FromData[WithdrawRedeemer] = FromData.derived
    given ToData[WithdrawRedeemer] = ToData.derived

    // Errors
    // This doesn't inline
    enum Error(msg: String):
        case SomeErr1 extends Error("SomeErr1")
    // This inlines well
    inline val SomeErr2 = "SomeErr2"

    override def spend(datum: Option[Data], redeemer: Data, tx: TxInfo, ownRef: TxOutRef): Unit =
        redeemer.to[TreasuryRedeemer] match
            case Resolve          => false orFail Error.SomeErr1.toString
            case Withdraw(_proof) => false orFail SomeErr2
            case Deinit           => false orFail SomeErr2

    @Ignore
    val script: Program = compile(TreasuryValidator.validate)
        .toUplc(generateErrorTraces = true)
        .plutusV3

    @Ignore
    def showSir(): Unit = println(compile(TreasuryValidator.validate).showHighlighted)

end TreasuryValidator

def foo() = 42
