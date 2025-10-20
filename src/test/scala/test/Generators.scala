package test

import hydrozoa.lib.tx.TransactionUnspentOutput
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.tx.Tx
import hydrozoa.multisig.ledger.dapp.tx.Tx.Builder.Config
import org.scalacheck.{Arbitrary, Gen}
import scalus.cardano.address.Network
import scalus.cardano.ledger.TransactionInput
import scalus.cardano.ledger.rules.STS.Validator
import scalus.cardano.ledger.txbuilder.{Environment}
import scalus.cardano.ledger.ArbitraryInstances.{*, given}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.TransactionOutput.Babbage

/**
 * This module contains shared generators and arbitrary instances that may be shared among multiple
 * tests. We separate them into "Hydrozoa" and "Other" objects for ease of upstreaming.
 */

object Generators {

    /**
     * NOTE: generators here are opinionated. They are not directly suitable for upstreaming
     * and contain reasonable, hydrozoa-specific defaults.
     */
    object Hydrozoa {
        def genTxConfig(env : Environment = testTxBuilderEnvironment,
                        validators : Seq[Validator] = testValidators) : Gen[Tx.Builder.Config] =
            for {
                peers <- genTestPeers
                hns = HeadMultisigScript(peers.map(_.wallet.exportVerificationKeyBytes))
                multisigWitnessUtxo <- genFakeMultisigWitnessUtxo(hns, env.network)
            } yield Config(hns, multisigWitnessUtxo, env, validators)

        
        def genFakeMultisigWitnessUtxo(
                                          script: HeadMultisigScript,
                                          network: Network
                                      ): Gen[TransactionUnspentOutput] = for {
            utxoId <- Arbitrary.arbitrary[TransactionInput]
            output = Babbage(
                script.mkAddress(network),
                Value.ada(2),
                None,
                Some(ScriptRef.apply(script.script))
            )
        } yield TransactionUnspentOutput((utxoId, output))
    
    }
}
