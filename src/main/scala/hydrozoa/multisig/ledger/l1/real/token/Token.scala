package hydrozoa.multisig.ledger.l1.real.token

import hydrozoa.{Utxo, L1}
import scalus.cardano.ledger.AssetName

object Token {
    object CIP67Tags {
        val head: Long = 4937L // "HYDR" (hydrozoa) on the phone pad
        val rollout: Long = 7655L // "ROLL" (rollout) on the phone pad
        val vote: Long = 8683L // "VOTE" (dispute) on the phone pad
    }

    def mkHeadTokenName(fundingUtxos: List[Utxo[L1]]): AssetName = {
        // Concatenate the CIP-67 treasury token name prefix with the hash of the list of funding utxos
        ???
    }

    val rolloutTokenName: AssetName = ???

    val voteTokenName: AssetName = ???
}
