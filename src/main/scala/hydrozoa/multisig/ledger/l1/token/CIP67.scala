package hydrozoa.multisig.ledger.l1.token
import com.bloxbean.cardano.client.cip.cip67
import io.bullet.borer.Cbor
import scalus.cardano.ledger.{AssetName, TransactionInput}
import scalus.uplc.builtin.Builtins.blake2b_224
import scalus.uplc.builtin.ByteString;

object CIP67 {
    object Tags {
        val head: Int = 4937 // "HYDR" (hydrozoa) on the phone pad
        val vote: Int = 8683 // "VOTE" (vote) on the phone pad
        val multiSigRegime: Int =
            4679 // "HMRW" (hydrozoa multisig regime witness) on the phone pad
    }

    case class HeadTokenNames(seedUtxo: TransactionInput) {
        private val tokenSuffix: Array[Byte] = {
            // Serialized + hashed utxo ID of seed utxo
            val utxoBytes = ByteString.fromArray(Cbor.encode(seedUtxo).toByteArray)
            blake2b_224(utxoBytes).bytes
        }

        private def prefixToken(cip67Tag: Int): AssetName =
            AssetName(
              ByteString.fromArray(cip67.CIP67AssetNameUtil.labelToPrefix(cip67Tag) ++ tokenSuffix)
            )

        val treasuryTokenName: AssetName = prefixToken(CIP67.Tags.head)

        val voteTokenName: AssetName = prefixToken(CIP67.Tags.vote)

        val multisigRegimeTokenName: AssetName = prefixToken(CIP67.Tags.multiSigRegime)
    }

    trait HasTokenNames {
        def headTokenNames: HeadTokenNames
    }

}
