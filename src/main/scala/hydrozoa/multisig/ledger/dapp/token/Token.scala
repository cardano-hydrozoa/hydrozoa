package hydrozoa.multisig.ledger.dapp.token

import scalus.cardano.ledger.{AssetName, TransactionInput}

object Token {
    object CIP67Tags {
        val head: Long = 4937L // "HYDR" (hydrozoa) on the phone pad
        val rollout: Long = 7655L // "ROLL" (rollout) on the phone pad
        val vote: Long = 8683L // "VOTE" (dispute) on the phone pad
    }

    def mkHeadTokenName(fundingUtxos: List[(TransactionInput)]): AssetName = {
        // Concatenate the CIP-67 treasury token name prefix with the hash of the list of funding utxos
        ???
    }

    val rolloutTokenName: AssetName = ???

    val voteTokenName: AssetName = ???
}
