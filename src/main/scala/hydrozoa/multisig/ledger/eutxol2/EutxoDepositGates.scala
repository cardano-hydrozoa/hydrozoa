package hydrozoa.multisig.ledger.eutxol2

import cats.syntax.all.*
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.multisig.ledger.eutxol2.tx.L2Genesis
import hydrozoa.multisig.ledger.joint.obligation.Payout
import scalus.cardano.ledger.Value

/** The two registration-time deposit-validity gates, shared by the command path
  * ([[EutxoL2Ledger.registerDeposit]]) and screening ([[EutxoL2Screener.screenDeposit]]). Pure
  * functions of the deposit's spawned genesis and the config; kept off
  * `EutxoL2Ledger.applyMutation` so `restoreTo`'s replay stays a pure reconstruction and never
  * re-litigates validity. A deposit that passes both at registration is guaranteed to absorb.
  */
object EutxoDepositGates:

    /** Each spawned L2 output must clear min-ada on its own — the same [[Payout.Obligation]] check
      * absorption applies. A sub-min-ada output that slipped past would otherwise fail only at
      * absorption (which cannot reject) and wedge the block on every recovery re-drive.
      */
    def validateSpawnedOutputs(
        l2Genesis: L2Genesis,
        config: CardanoNetwork.Section
    ): Either[String, Unit] =
        l2Genesis.asUtxos.values.toVector
            .traverse(output => Payout.Obligation(output, config))
            .map(_ => ())
            .leftMap(e => s"deposit spawns an invalid L2 output: $e")

    /** The value-conservation gate: a deposit's spawned L2 outputs must equal `depositL2Value` (the
      * L1 value the treasury absorbs) exactly — in the coin and in every asset. Spawning more would
      * mint L2 value (the depositor is over-credited); spawning less would silently donate the
      * difference to the treasury (the depositor is under-credited, and the value never enters the
      * evacuation map). Either direction breaks the head's conservation invariant (the map's total
      * equals absorbed deposits minus payouts).
      */
    def validateDepositConservation(
        l2Genesis: L2Genesis,
        depositL2Value: Value
    ): Either[String, Unit] =
        val spawnedValue = Value.combine(l2Genesis.genesisObligations.map(_.l2OutputValue))
        Either.cond(
          (depositL2Value - spawnedValue).isZero,
          (),
          s"deposit l2Payload outputs ($spawnedValue) must equal depositL2Value ($depositL2Value)"
        )
