package hydrozoa.multisig.ledger.stack

import hydrozoa.multisig.ledger.stack.PartitionEffects.{Final, Major, Minor}
import scalus.cardano.ledger.TransactionHash
import scalus.uplc.builtin.{ByteString, platform}

/** L1 identity of the effects a hard-confirmed stack carries. Every effect is addressed by a single
  * `l1TxId`:
  *   - a real L1 tx effect (initialization, settlement, fallback, rollout, finalization, post-dated
  *     refund) is its `EnrichedTx`'s `tx.id`;
  *   - a standalone evacuation commitment (SEC) is not an L1 tx, so it takes a **synthetic**
  *     `l1TxId = blake2b_256(sec.header)` — the same 32-byte shape as a real tx id, over the SEC's
  *     on-chain serialized bytes (never a real tx-body preimage, so it cannot collide in practice).
  *
  * These are the ids the effect queries resolve and the reverse index (`Cf.EffectStack`) keys.
  */
object EffectIds:

    /** The synthetic l1TxId for a standalone evacuation commitment. */
    def secL1TxId(sec: StandaloneEvacuationCommitment): TransactionHash =
        val headerBytes: Array[Byte] = sec.header
        TransactionHash.fromByteString(platform.blake2b_256(ByteString.fromArray(headerBytes)))

    /** Every effect's l1TxId in a hard-confirmed stack, in a stable order (partition order, then
      * within a partition the settlement/finalization opener, its rollouts, refunds, then the SEC).
      * Used to write the reverse index at hard-confirmation time.
      */
    def allL1TxIds(hc: StackEffects.HardConfirmed): List[TransactionHash] = hc match
        case i: StackEffects.HardConfirmed.Initial =>
            List(i.initializationTx.tx.id, i.fallbackTx.tx.id)
        case r: StackEffects.HardConfirmed.Regular =>
            r.partitions.toList.flatMap {
                case p: Major[StandaloneEvacuationCommitment.MultiSigned] =>
                    List(p.settlement.tx.id, p.fallback.tx.id) ++
                        p.rollouts.map(_.tx.id) ++
                        p.refunds.map(_.tx.id) ++
                        p.sec.map(ms => secL1TxId(ms.commitment)).toList
                case p: Final =>
                    p.finalization.tx.id :: p.rollouts.map(_.tx.id)
                case p: Minor[StandaloneEvacuationCommitment.MultiSigned] =>
                    secL1TxId(p.sec.commitment) :: p.refunds.map(_.tx.id)
            }
