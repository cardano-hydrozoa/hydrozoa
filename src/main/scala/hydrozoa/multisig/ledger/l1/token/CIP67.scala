package hydrozoa.multisig.ledger.l1.token
import com.bloxbean.cardano.client.cip.cip67
import scalus.cardano.ledger.AssetName
import scalus.uplc.builtin.ByteString;

object CIP67 {
    object Tags {
        val head: Int = 4937 // "HYDR" (hydrozoa) on the phone pad
        val vote: Int = 8683 // "VOTE" (vote) on the phone pad
        val regimeWitness: Int =
            4798 // "HRWT" (Hydrozoa regime witness token) on the phone pad
    }

    def prefix(cip67Tag: Int): ByteString =
        ByteString.fromArray(cip67.CIP67AssetNameUtil.labelToPrefix(cip67Tag))

    /** The head's three CIP-67 token names. All three share the same 28-byte [[suffix]]
      * (`blake2b_224` of the CBOR-encoded seed input) and differ only in their 4-byte CIP-67 label
      * prefix, so any one name determines the others. The seed → suffix derivation lives in the
      * `hydrozoa.bootstrap` module (config authoring); the running head reconstructs the names from
      * its explicitly-stored head id via [[HeadTokenNames.fromHeadId]].
      */
    case class HeadTokenNames private (suffix: ByteString) {
        val treasuryTokenName: AssetName = AssetName(prefix(CIP67.Tags.head) ++ suffix)

        val voteTokenName: AssetName = AssetName(prefix(CIP67.Tags.vote) ++ suffix)

        val regimeWitnessTokenName: AssetName = AssetName(
          prefix(CIP67.Tags.regimeWitness) ++ suffix
        )
    }

    object HeadTokenNames {

        /** Build from the shared 28-byte suffix. Used by the bootstrap head-id calculation. */
        def fromSuffix(suffix: ByteString): HeadTokenNames = new HeadTokenNames(suffix)

        /** Reconstruct all three token names from the head id (the treasury token name), by
          * stripping the head prefix to recover the shared suffix.
          */
        def fromHeadId(treasuryTokenName: AssetName): HeadTokenNames =
            fromSuffix(
              ByteString.fromHex(
                treasuryTokenName.bytes.toHex.drop(prefix(CIP67.Tags.head).toHex.length)
              )
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
        println(s"regime witness = ${CIP67.prefix(CIP67.Tags.regimeWitness)}")
    }
}
