package hydrozoa.multisig.ledger.dapp.token
import scalus.builtin.Builtins.blake2b_224
import scalus.builtin.ByteString
import scalus.cardano.ledger.{AssetName, TransactionInput}

// FIXME: This is not actually CIP-67 compliant as is. See:
// https://github.com/cardano-hydrozoa/hydrozoa/issues/260
object CIP67 {
    object Tags {
        val head: Long = 4937L // "HYDR" (hydrozoa) on the phone pad
        val rollout: Long = 7655L // "ROLL" (rollout) on the phone pad
        val vote: Long = 8683L // "VOTE" (dispute) on the phone pad
        val multiSigRegime: Long =
            4679L // "HMRW" (hydrozoa multisig regime witness) on the phone pad
    }

    class TokenNames(seedUtxo: TransactionInput) {
        private val tokenSuffix: Array[Byte] = {
            // Serialized + hashed utxo ID of seed utxo
            val utxoBytes = ByteString.fromArray(
              seedUtxo.transactionId.bytes ++ BigInt(seedUtxo.index).toByteArray
            )
            blake2b_224(utxoBytes).bytes
        }

        private def prefixToken(cip67Tag: Long): AssetName =
            AssetName(ByteString.fromArray(BigInt(cip67Tag).toByteArray ++ tokenSuffix))

        val headTokenName: AssetName = prefixToken(CIP67.Tags.head)

        val voteTokenName: AssetName = prefixToken(CIP67.Tags.vote)

        val rolloutTokenName: AssetName = prefixToken(CIP67.Tags.rollout)

        val multisigRegimeTokenName: AssetName = prefixToken(CIP67.Tags.multiSigRegime)
    }

}
