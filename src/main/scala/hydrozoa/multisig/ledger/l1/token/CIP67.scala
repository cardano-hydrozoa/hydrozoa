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

    def prefix(cip67Tag: Int): ByteString =
        ByteString.fromArray(cip67.CIP67AssetNameUtil.labelToPrefix(cip67Tag))

    case class HeadTokenNames(seedUtxo: TransactionInput) {
        private val suffix: ByteString = {
            // Serialized + hashed utxo ID of seed utxo
            val utxoBytes = ByteString.fromArray(Cbor.encode(seedUtxo).toByteArray)
            blake2b_224(utxoBytes)
        }

        val treasuryTokenName: AssetName = AssetName(prefix(CIP67.Tags.head) ++ suffix)

        val voteTokenName: AssetName = AssetName(prefix(CIP67.Tags.vote) ++ suffix)

        val multisigRegimeTokenName: AssetName = AssetName(
          prefix(CIP67.Tags.multiSigRegime) ++ suffix
        )
    }

    trait HasTokenNames {
        def headTokenNames: HeadTokenNames
    }

}

object PrintPrefixes {
    def main(args: Array[String]): Unit = {
        println("=" * 80)
        println("Prefixes:")
        println(s"head = ${CIP67.prefix(CIP67.Tags.head)}")
        println(s"vote = ${CIP67.prefix(CIP67.Tags.vote)}")
        println(s"multisig regime = ${CIP67.prefix(CIP67.Tags.multiSigRegime)}")
    }
}
