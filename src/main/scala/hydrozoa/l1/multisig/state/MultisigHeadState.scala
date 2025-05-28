package hydrozoa.l1.multisig.state

import hydrozoa.{L1, TaggedUtxo, TaggedUtxoSet, Utxo, UtxoSet, TaggedUtxoSetMutable}

import scala.collection.mutable

/** This is L1 state as node's L1 provider sees it.
  *
  * @param treasuryUtxo
  * @param depositUtxos
  * @param rolloutUtxos
  */
case class MultisigHeadStateL1(
    var treasuryUtxo: TreasuryUtxo,
    depositUtxos: DepositUtxosMutable,
    // FIXME: move to rollout effect (?)
    rolloutUtxos: RolloutUtxosMutable
)

object MultisigHeadStateL1:
    def apply(treasuryUtxo: TreasuryUtxo): MultisigHeadStateL1 =
        MultisigHeadStateL1(
          treasuryUtxo,
          TaggedUtxoSetMutable[L1, DepositTag](mutable.Map.empty),
          TaggedUtxoSetMutable[L1, RolloutTag](mutable.Map.empty)
        )

type TreasuryUtxo = TaggedUtxo[L1, TreasuryTag]

// tags
sealed trait MultisigUtxoTag
sealed trait DepositTag extends MultisigUtxoTag
sealed trait RolloutTag extends MultisigUtxoTag
sealed trait TreasuryTag extends MultisigUtxoTag

// Additional stuff

type DepositUtxo = TaggedUtxo[L1, DepositTag]
type DepositUtxos = TaggedUtxoSet[L1, DepositTag]
type DepositUtxosMutable = TaggedUtxoSetMutable[L1, DepositTag]

type RolloutUtxo = TaggedUtxo[L1, RolloutTag]
type RolloutUtxos = TaggedUtxoSet[L1, RolloutTag]
type RolloutUtxosMutable = TaggedUtxoSetMutable[L1, RolloutTag]
