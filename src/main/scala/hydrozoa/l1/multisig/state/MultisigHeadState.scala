package hydrozoa.l1.multisig.state

import hydrozoa.{L1, Utxo, UtxoSet, UtxoSetMutable}

import scala.collection.mutable

/** FIXME: make it immutable!
  *
  * This is L1 state as node's L1 provider sees it.
  *
  * @param treasuryUtxo
  * @param depositUtxos
  * @param rolloutUtxos
  */
case class MultisigHeadStateL1(
    var treasuryUtxo: TreasuryUtxo,
    depositUtxos: DepositUtxosMutable,
    // FIXME: move to rollout effect
    rolloutUtxos: RolloutUtxosMutable
)

object MultisigHeadStateL1:
    def apply(treasuryUtxo: TreasuryUtxo): MultisigHeadStateL1 =
        MultisigHeadStateL1(
          treasuryUtxo,
          UtxoSetMutable[L1, DepositTag](mutable.Map.empty),
          UtxoSetMutable[L1, RolloutTag](mutable.Map.empty)
        )

type TreasuryUtxo = Utxo[L1, TreasuryTag]

// tags
sealed trait MultisigUtxoTag
sealed trait DepositTag extends MultisigUtxoTag
sealed trait RolloutTag extends MultisigUtxoTag
sealed trait TreasuryTag extends MultisigUtxoTag

// Additional stuff

type DepositUtxo = Utxo[L1, DepositTag]
type DepositUtxos = UtxoSet[L1, DepositTag]
type DepositUtxosMutable = UtxoSetMutable[L1, DepositTag]

type RolloutUtxo = Utxo[L1, RolloutTag]
type RolloutUtxos = UtxoSet[L1, RolloutTag]
type RolloutUtxosMutable = UtxoSetMutable[L1, RolloutTag]
