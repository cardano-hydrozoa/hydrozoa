package hydrozoa.rulebased.evacuator

import hydrozoa.config.head.HeadConfig
import hydrozoa.multisig.ledger.joint.EvacuationMap
import hydrozoa.rulebased.ledger.l1.state.TreasuryState.RuleBasedTreasuryDatum.Resolved
import hydrozoa.rulebased.ledger.l1.utxo.{RuleBasedRegimeOutput, RuleBasedTreasuryOutput}
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.TransactionBuilder
import scalus.cardano.txbuilder.TransactionBuilderStep.{Mint, Send, Spend}

/** Builds the one transaction that puts a testable treasury on chain.
  *
  * It stands in for the fallback transaction a real head would have submitted, producing the same
  * two utxos the `Evacuate` validator reads: the rule-based regime utxo, holding the HRWT beacon
  * and the head-identity datum that authenticates the setup ladder; and the treasury itself,
  * holding the head beacon, the funds to pay out, and a **resolved** datum committing to our
  * evacuation map.
  *
  * The head's own minting policy is a native script we control, and the validator only checks that
  * the beacons are of the policy the datum declares. So no head has to have run — but everything
  * the validator inspects is genuine, which is the point: a transaction that passes here passes
  * because the real rules were satisfied, not because the setup was lenient.
  */
object SyntheticTreasuryTx {

    type Config = HeadConfig.Bootstrap.Section

    /** @param fundingUtxos
      *   wallet utxos to spend; must cover the payouts, both beacons' min-ada, and the fee
      * @param map
      *   the evacuation map to commit to — its KZG commitment becomes `evacuationActive`
      */
    def build(
        fundingUtxos: Utxos,
        map: EvacuationMap,
        changeAddress: scalus.cardano.address.ShelleyAddress
    )(using config: Config): Either[Any, TransactionBuilder.Context] = {

        val policy = config.headMultisigScript.policyId
        // The script travels by value on the first mint that needs it, and by reference after —
        // the builder's steps are not commutative, so attaching second would fail to resolve.
        val witnessFirst = config.headMultisigScript.witnessValue
        val witnessThen = config.headMultisigScript.witnessAttached

        // The treasury's datum is `Resolved`, which is the state `Evacuate` requires: a head that
        // has already voted and tallied. Version is arbitrary — nothing in the evacuation path
        // reads it, and no later transition depends on it here.
        val treasuryDatum = Resolved(
          headMp = policy,
          evacuationActive = map.kzgCommitment,
          version = (BigInt(1), BigInt(0))
        )

        val treasuryValue =
            Value(Coin(map.evacuationMap.values.map(_.utxo.value.value.coin.value).sum)) +
                Value(config.collectiveContingency.minAdaForTreasury) +
                Value.asset(policy, config.headTokenNames.treasuryTokenName, 1L)

        val treasury = RuleBasedTreasuryOutput(treasuryDatum, treasuryValue)

        val steps =
            fundingUtxos.toList.map { case (i, o) => Spend(Utxo(i, o)) } ++
                List(
                  // The head beacon: exactly one, in the treasury. `Evacuate` fails without it.
                  Mint(
                    policy,
                    assetName = config.headTokenNames.treasuryTokenName,
                    amount = 1L,
                    witness = witnessFirst
                  ),
                  // The regime beacon (HRWT). The validator locates the regime utxo by this token
                  // and reads the ladder anchor from its datum, so the ladder is authenticated
                  // through it rather than trusted from the transaction.
                  Mint(
                    policy,
                    assetName = config.headTokenNames.regimeWitnessTokenName,
                    amount = 1L,
                    witness = witnessThen
                  ),
                  RuleBasedRegimeOutput.send,
                  treasury.send,
                  Send(
                    TransactionOutput.Babbage(
                      address = changeAddress,
                      value = Value(Coin(0L)),
                      datumOption = None,
                      scriptRef = None
                    )
                  )
                )

        TransactionBuilder.build(config.network, steps)
    }
}
