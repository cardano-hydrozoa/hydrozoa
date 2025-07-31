package hydrozoa.l1.rulebased.onchain

import com.bloxbean.cardano.client.address.AddressProvider
import com.bloxbean.cardano.client.plutus.spec.PlutusV3Script
import hydrozoa.infra.{encodeHex, toBB}
import hydrozoa.{AddressBech, AddressBechL1, L1, Network}
import scalus.*
import scalus.builtin.*
import scalus.builtin.Builtins.*
import scalus.builtin.ByteString.hex
import scalus.ledger.api.v3.*
import scalus.prelude.crypto.bls12_381.{G1, G2}
import scalus.prelude.*

@Compile
object UnliftMinimalValidator extends Validator:
    // Entry point
    override def spend(datum: Option[Data], redeemer: Data, tx: TxInfo, ownRef: TxOutRef): Unit =

        log("TreasuryValidator")

        val g1 = G1.uncompress(
          hex"97f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb"
        )
        val g2 = G2.uncompress(
          hex"b0f15b32629d02514af939e5b660d27a4db9f84cde5eecfef7db87c056163a9f21925653519cf9972f4b6c115e195baf1439203af99d121fce39ec8eed3fa72a0a31dd537642ab7cb1da52dfbacab1a032c5579aa702a59f1991e9aefae1d9c5"
        )
        // This ruins the script
        val whatever = bls12_381_millerLoop(g1, g2)

end UnliftMinimalValidator

object UnliftMinimialValidatorScript {
    val sir = Compiler.compile(UnliftMinimalValidator.validate)
//    val script = sir.toUplcOptimized(generateErrorTraces = true).plutusV3
    val script = sir.toUplc().plutusV3

    // TODO: can we use Scalus for that?
    val plutusScript: PlutusV3Script = PlutusV3Script
        .builder()
        .`type`("PlutusScriptV3")
        .cborHex(script.doubleCborHex)
        .build()
        .asInstanceOf[PlutusV3Script]

    val scriptHash: ByteString = ByteString.fromArray(plutusScript.getScriptHash)

    val scriptHashString: String = encodeHex(IArray.unsafeFromArray(plutusScript.getScriptHash))

    def address(n: Network): AddressBechL1 = {
        val address = AddressProvider.getEntAddress(plutusScript, n.toBB)
        address.getAddress |> AddressBech[L1].apply
    }
}

@main
def minimalValidatorSir(args: String): Unit =
    println(UnliftMinimialValidatorScript.scriptHashString)
    println(UnliftMinimialValidatorScript.scriptHash)
    println(UnliftMinimialValidatorScript.script.flatEncoded.length)
