package hydrozoa.multisig.ledger.dapp.token

import cats.data.NonEmptyList
import scalus.builtin.Builtins.blake2b_224
import scalus.builtin.ByteString
import scalus.cardano.ledger.{AssetName, TransactionInput}

object Token {
    object CIP67Tags {
        val head: Long = 4937L // "HYDR" (hydrozoa) on the phone pad
        val rollout: Long = 7655L // "ROLL" (rollout) on the phone pad
        val vote: Long = 8683L // "VOTE" (dispute) on the phone pad
        val dispute: Long = 3477 // "DISP"
    }

    private def mkTokenSuffix(seedUtxos: NonEmptyList[TransactionInput]): Array[Byte] = {
        // Concatenate the CIP-67 treasury token name prefix with the hash of the list of seed utxos
        val utxoBytes = ByteString.fromArray(
          seedUtxos.toList
              .flatMap(ti => ti.transactionId.bytes ++ BigInt(ti.index).toByteArray)
              .toArray
        )
        blake2b_224(utxoBytes).bytes
    }

    private def prefixToken(cip67Tag: Long, seedUtxos: NonEmptyList[TransactionInput]): AssetName =
        AssetName(ByteString.fromArray(BigInt(cip67Tag).toByteArray ++ mkTokenSuffix(seedUtxos)))

    def mkHeadTokenName(seedUtxos: NonEmptyList[(TransactionInput)]): AssetName = {
        prefixToken(CIP67Tags.head, seedUtxos)
    }

    // FIXME: This is wrong
    def mkVoteTokenName(multisigTreasuryInput : TransactionInput): AssetName =
        AssetName(ByteString.fromBigIntBigEndian(BigInt(CIP67Tags.dispute),2))
}
