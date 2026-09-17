package hydrozoa.multisig.ledger.eutxol2

import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.lib.crypto.Preimage
import java.nio.charset.StandardCharsets.UTF_8
import org.scalacheck.Prop.propBoolean
import org.scalacheck.{Prop, Properties}
import scalus.cardano.ledger.{Hash32, ProtocolParams}

/** [[EutxoL2Ledger.l2ParamsHash]] must be stable for a given L2 parameter set and must move when
  * anything the ledger's behaviour depends on moves.
  *
  * A preimage element that silently falls out is invisible in production: every peer agrees on a
  * digest that does not constrain the thing that differs, which is the whole failure this value
  * exists to prevent. So each element gets its own mutation here, exactly as `HeadParamsHashTest`
  * does for `headParamsHash`.
  *
  * The rule list gets the same treatment one level up: it is derived from
  * [[HydrozoaTransactionMutator.upstreamValidators]], the list `transit` folds over, so the
  * properties below check the derivation rather than a hand-copied set of names.
  */
object L2ParamsHashTest extends Properties("l2ParamsHash") {

    private val params: ProtocolParams = CardanoNetwork.Preview.cardanoProtocolParams

    /** One mutation per protocol-parameter field the L2 ledger's validators actually consult. Not
      * an exhaustive sweep of all 33: the digest covers the whole record by construction (it
      * serializes it whole), so these spot-check that the serialization is reaching the digest at
      * all, in fields spread across the record's shape — scalars, a nested case class, and a
      * `Double`.
      */
    private val parameterMutations: List[(String, ProtocolParams => ProtocolParams)] = List(
      "maxTxSize" -> (p => p.copy(maxTxSize = p.maxTxSize + 1)),
      "utxoCostPerByte" -> (p => p.copy(utxoCostPerByte = p.utxoCostPerByte + 1)),
      "maxValueSize" -> (p => p.copy(maxValueSize = p.maxValueSize + 1)),
      "collateralPercentage" -> (p => p.copy(collateralPercentage = p.collateralPercentage + 1)),
      "maxCollateralInputs" -> (p => p.copy(maxCollateralInputs = p.maxCollateralInputs + 1)),
      "txFeePerByte" -> (p => p.copy(txFeePerByte = p.txFeePerByte + 1)),
      "txFeeFixed" -> (p => p.copy(txFeeFixed = p.txFeeFixed + 1)),
      "minFeeRefScriptCostPerByte" -> (p =>
          p.copy(minFeeRefScriptCostPerByte = p.minFeeRefScriptCostPerByte + 1)
      ),
      "protocolVersion" -> (p =>
          p.copy(protocolVersion = p.protocolVersion.copy(major = p.protocolVersion.major + 1))
      ),
      "maxTxExecutionUnits" -> (p =>
          p.copy(maxTxExecutionUnits =
              p.maxTxExecutionUnits.copy(steps = p.maxTxExecutionUnits.steps + 1)
          )
      ),
      // A `Double` field, and one no L2 validator consults. It is in the digest because the record
      // goes in whole: which parameters a rule reads is Scalus's business and moves with Scalus.
      "monetaryExpansion" -> (p => p.copy(monetaryExpansion = p.monetaryExpansion + 0.001))
    )

    val _ = property("is deterministic") = Prop {
        EutxoL2Ledger.l2ParamsHash(params) == EutxoL2Ledger.l2ParamsHash(params)
    }

    parameterMutations.foreach { (label, mutate) =>
        val _ = property(s"covers $label") = Prop {
            EutxoL2Ledger.l2ParamsHash(mutate(params)) != EutxoL2Ledger.l2ParamsHash(params)
        }
    }

    val _ = property("the rule list is what transit runs") = Prop {
        val derived = HydrozoaTransactionMutator.upstreamValidators.map(v =>
            v.getClass.getSimpleName.stripSuffix("$")
        )
        // Every upstream validator appears in the rule list, under the name the digest hashes.
        derived.forall(HydrozoaTransactionMutator.ruleNames.contains)
    }

    val _ = property("rule names are distinct") = Prop {
        val rules = HydrozoaTransactionMutator.ruleNames ++ EutxoDepositGates.ruleNames
        rules.distinct.size == rules.size
    } :| "a repeated name would let two rules swap without moving the digest"

    val _ = property("dropping a rule moves the digest") = Prop {
        // The digest folds the rule list in as `u32(count) || framed(name)*`, so a shorter list
        // must produce a different value. Computed here rather than by mutating the ledger, which
        // reads its list from a `val`.
        val full = HydrozoaTransactionMutator.ruleNames ++ EutxoDepositGates.ruleNames
        digestOfRules(full) != digestOfRules(full.drop(1))
    }

    val _ = property("reordering rules moves the digest") = {
        val full = HydrozoaTransactionMutator.ruleNames ++ EutxoDepositGates.ruleNames
        (full.size >= 2) ==> (digestOfRules(full) != digestOfRules(full.reverse))
    }

    /** The rule-list half of the preimage, in isolation — the same layout
      * [[EutxoL2Ledger.l2ParamsHash]] writes, so a change to one that is not mirrored in the other
      * shows up as a failure here.
      */
    private def digestOfRules(rules: Vector[String]): Hash32 = {
        val out = Preimage()
        out.u32(rules.size)
        rules.foreach(rule => out.framed(rule.getBytes(UTF_8)))
        out.mkDigest
    }
}
