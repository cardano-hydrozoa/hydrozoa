package hydrozoa.multisig.server

import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all.*
import hydrozoa.multisig.consensus.UserRequestWithId
import hydrozoa.multisig.ledger.block.{BlockBrief, BlockNumber}
import hydrozoa.multisig.ledger.event.RequestId
import hydrozoa.multisig.ledger.l1.tx.RefundTx
import hydrozoa.multisig.ledger.stack.PartitionEffects.{Final, Major, Minor}
import hydrozoa.multisig.ledger.stack.{EffectIds, PartitionEffects, StackEffects, StackNumber, StandaloneEvacuationCommitment}
import hydrozoa.multisig.persistence.ConsensusStoreReader
import scalus.cardano.ledger.{Transaction, TransactionHash}

/** The kind of L1 effect an entry represents. */
enum EffectKind:
    case Initialization, Settlement, Fallback, Rollout, Finalization, Refund, Sec

/** One L1 effect a hard-confirmed stack carries, attributed to the single block it belongs to and
  * addressed by its `l1TxId` (a real tx id, or the SEC's synthetic hash).
  */
sealed trait ResolvedEffect:
    def l1TxId: TransactionHash
    def blockNumber: BlockNumber
    def kind: EffectKind

object ResolvedEffect:
    /** A real L1 tx effect — its `l1TxId` is the tx id. `rolloutIndex` positions a rollout within
      * its block's rollout chain (`None` for non-rollout effects).
      */
    final case class Tx(
        l1TxId: TransactionHash,
        blockNumber: BlockNumber,
        kind: EffectKind,
        tx: Transaction,
        rolloutIndex: Option[Int]
    ) extends ResolvedEffect

    /** A standalone evacuation commitment — not a real tx, so its `l1TxId` is synthetic. */
    final case class Sec(
        l1TxId: TransactionHash,
        blockNumber: BlockNumber,
        commitment: StandaloneEvacuationCommitment.MultiSigned
    ) extends ResolvedEffect:
        def kind: EffectKind = EffectKind.Sec

/** Decomposes a hard-confirmed stack's **per-partition** effects onto the individual **blocks**
  * they belong to — the bridge the block-effects queries cross, since effects are produced and
  * persisted per stack/partition but the API addresses them per block.
  *
  * Attribution: an SEC self-identifies its block (`commitment.blockNum`); a settlement / fallback /
  * finalization and its rollouts belong to their partition's opener block (the Major or Final
  * block); a post-dated refund belongs to the block where its deposit was registered (via the
  * request → block index). Stack 0 (`Initial`) is the initialization + initial-fallback pair on
  * block 0.
  */
final class EffectsResolver(reader: ConsensusStoreReader[IO]):

    /** The effects attributed to one block, or `None` when the block does not exist. A block that
      * exists but is not yet hard-confirmed has an empty effect list.
      */
    def blockEffects(num: BlockNumber): IO[Option[List[ResolvedEffect]]] =
        val existsAndEffects: IO[Option[List[ResolvedEffect]]] =
            reader
                .stackOf(num)
                .flatMap {
                    case Some(stackNum) =>
                        resolveStack(stackNum).map(es => es.filter(_.blockNumber == num))
                    case None => IO.pure(Nil)
                }
                .map(Some(_))
        if num == BlockNumber.zero then existsAndEffects
        else
            reader.blockBrief(num).flatMap {
                case None    => IO.pure(None)
                case Some(_) => existsAndEffects
            }

    /** The single effect with this `l1TxId`, if the store has one (via the effect → stack index).
      */
    def effectById(l1TxId: TransactionHash): IO[Option[ResolvedEffect]] =
        reader.effectStack(l1TxId).flatMap {
            case None           => IO.pure(None)
            case Some(stackNum) => resolveStack(stackNum).map(_.find(_.l1TxId == l1TxId))
        }

    /** The L1 effects a request became — addressed by `l1TxId`, empty until the covering stack is
      * hard-confirmed.
      *
      * A transaction's effects are the carriers of its inclusion-block partition: the SEC for a
      * minor partition, the settlement for a major, the finalization for the final. A deposit's
      * effects are its post-dated refund (matched by the `requestId` the refund carries, in the
      * registration-block partition) and, once absorbed, the settlement of its absorbing block's
      * partition (via the deposit-absorption index).
      */
    def relatedEffects(requestId: RequestId): IO[List[ResolvedEffect]] =
        reader.request(requestId).flatMap {
            case None => IO.pure(Nil)
            case Some(stamped) =>
                stamped.payload match
                    case _: UserRequestWithId.TransactionRequest => transactionEffects(requestId)
                    case _: UserRequestWithId.DepositRequest     => depositEffects(requestId)
        }

    /** A transaction's carriers: the effects of its inclusion-block partition. */
    private def transactionEffects(requestId: RequestId): IO[List[ResolvedEffect]] =
        reader.requestBlock(requestId).flatMap {
            case None => IO.pure(Nil)
            case Some(entry) =>
                partitionForBlock(entry.blockNum).map {
                    case None                  => Nil
                    case Some((blockNums, pe)) => carrierEffects(blockNums.head, pe)
                }
        }

    /** A deposit's effects: its post-dated refund (registration-block partition) and, once
      * absorbed, the settlement of its absorbing block's partition.
      */
    private def depositEffects(requestId: RequestId): IO[List[ResolvedEffect]] =
        val refund: IO[List[ResolvedEffect]] =
            reader.requestBlock(requestId).flatMap {
                case None => IO.pure(Nil)
                case Some(entry) =>
                    partitionForBlock(entry.blockNum).map {
                        case None => Nil
                        case Some((_, pe)) =>
                            depositRefundEffect(requestId, entry.blockNum, pe).toList
                    }
            }
        val settlement: IO[List[ResolvedEffect]] =
            reader.absorptionBlock(requestId).flatMap {
                case None => IO.pure(Nil)
                case Some(block) =>
                    partitionForBlock(block).map {
                        case None                  => Nil
                        case Some((blockNums, pe)) => settlementEffect(blockNums.head, pe).toList
                    }
            }
        (refund, settlement).mapN(_ ++ _)

    /** Every effect a hard-confirmed stack carries, attributed per block. Empty when the stack is
      * not hard-confirmed.
      */
    def resolveStack(stackNum: StackNumber): IO[List[ResolvedEffect]] =
        reader.hardConfirmation(stackNum).flatMap {
            case None => IO.pure(Nil)
            case Some(timestamped) =>
                timestamped.payload match
                    case i: StackEffects.HardConfirmed.Initial =>
                        IO.pure(
                          List(
                            ResolvedEffect
                                .Tx(
                                  i.initializationTx.tx.id,
                                  BlockNumber.zero,
                                  EffectKind.Initialization,
                                  i.initializationTx.tx,
                                  None
                                ),
                            ResolvedEffect.Tx(
                              i.fallbackTx.tx.id,
                              BlockNumber.zero,
                              EffectKind.Fallback,
                              i.fallbackTx.tx,
                              None
                            )
                          )
                        )
                    case r: StackEffects.HardConfirmed.Regular =>
                        resolveRegular(stackNum, r)
        }

    private def resolveRegular(
        stackNum: StackNumber,
        effects: StackEffects.HardConfirmed.Regular
    ): IO[List[ResolvedEffect]] =
        for {
            briefs <- stackBriefs(stackNum)
            groups = partitionGroups(briefs)
            resolved <-
                if groups.length != effects.partitions.length then
                    IO.raiseError(
                      new IllegalStateException(
                        s"stack $stackNum: ${groups.length} block partitions but " +
                            s"${effects.partitions.length} effect partitions"
                      )
                    )
                else
                    groups
                        .zip(effects.partitions.toList)
                        .flatTraverse((blockNums, pe) => resolvePartition(blockNums, pe))
        } yield resolved

    private def resolvePartition(
        blockNums: NonEmptyList[BlockNumber],
        pe: hydrozoa.multisig.ledger.stack.PartitionEffects[
          StandaloneEvacuationCommitment.MultiSigned
        ]
    ): IO[List[ResolvedEffect]] =
        val opener = blockNums.head
        pe match
            case p: Major[StandaloneEvacuationCommitment.MultiSigned] =>
                val opening = ResolvedEffect
                    .Tx(p.settlement.tx.id, opener, EffectKind.Settlement, p.settlement.tx, None) ::
                    ResolvedEffect
                        .Tx(p.fallback.tx.id, opener, EffectKind.Fallback, p.fallback.tx, None) ::
                    rolloutEffects(opener, p.rollouts)
                val sec = p.sec.toList.map(ms =>
                    ResolvedEffect
                        .Sec(EffectIds.secL1TxId(ms.commitment), ms.commitment.blockNum, ms)
                )
                p.refunds.traverse(attributeRefund(_, opener)).map(opening ++ sec ++ _)
            case p: Final =>
                IO.pure(
                  ResolvedEffect
                      .Tx(
                        p.finalization.tx.id,
                        opener,
                        EffectKind.Finalization,
                        p.finalization.tx,
                        None
                      )
                      :: rolloutEffects(opener, p.rollouts)
                )
            case p: Minor[StandaloneEvacuationCommitment.MultiSigned] =>
                val sec = ResolvedEffect
                    .Sec(EffectIds.secL1TxId(p.sec.commitment), p.sec.commitment.blockNum, p.sec)
                p.refunds.traverse(attributeRefund(_, opener)).map(sec :: _)

    private def rolloutEffects(
        block: BlockNumber,
        rollouts: List[hydrozoa.multisig.ledger.l1.tx.RolloutTx]
    ): List[ResolvedEffect] =
        rollouts.zipWithIndex.map((rt, i) =>
            ResolvedEffect.Tx(rt.tx.id, block, EffectKind.Rollout, rt.tx, Some(i))
        )

    /** A post-dated refund belongs to the block where its deposit was registered (the request →
      * block index); `fallbackBlock` is used only if that index entry is somehow absent.
      */
    private def attributeRefund(refund: RefundTx, fallbackBlock: BlockNumber): IO[ResolvedEffect] =
        val block: IO[BlockNumber] = refund match
            case p: RefundTx.PostDated =>
                reader
                    .requestBlock(p.requestId)
                    .map(_.map(_.blockNum).getOrElse(fallbackBlock))
        block.map(b => ResolvedEffect.Tx(refund.tx.id, b, EffectKind.Refund, refund.tx, None))

    /** The block-number group and per-partition effects of the partition containing `block`, or
      * `None` when the block's stack is not hard-confirmed (or has no such partition). Needs the
      * stack's briefs to reconstruct the partition grouping, so an unbriefed stack yields `None`.
      */
    private def partitionForBlock(
        block: BlockNumber
    ): IO[Option[
      (NonEmptyList[BlockNumber], PartitionEffects[StandaloneEvacuationCommitment.MultiSigned])
    ]] =
        reader.stackOf(block).flatMap {
            case None => IO.pure(None)
            case Some(stackNum) =>
                stackBriefs(stackNum).flatMap { briefs =>
                    if briefs.isEmpty then IO.pure(None)
                    else
                        reader.hardConfirmation(stackNum).map {
                            case None => None
                            case Some(timestamped) =>
                                timestamped.payload match
                                    case r: StackEffects.HardConfirmed.Regular =>
                                        val groups = partitionGroups(briefs)
                                        if groups.length != r.partitions.length then None
                                        else
                                            groups
                                                .zip(r.partitions.toList)
                                                .find((g, _) => g.toList.contains(block))
                                    case _: StackEffects.HardConfirmed.Initial => None
                        }
                }
        }

    /** A transaction's L1 carriers in its partition: the settlement (Major, plus its SEC), the
      * finalization (Final), or the SEC (Minor).
      */
    private def carrierEffects(
        opener: BlockNumber,
        pe: PartitionEffects[StandaloneEvacuationCommitment.MultiSigned]
    ): List[ResolvedEffect] =
        pe match
            case p: Major[StandaloneEvacuationCommitment.MultiSigned] =>
                ResolvedEffect
                    .Tx(p.settlement.tx.id, opener, EffectKind.Settlement, p.settlement.tx, None) ::
                    p.sec.toList.map(ms =>
                        ResolvedEffect
                            .Sec(EffectIds.secL1TxId(ms.commitment), ms.commitment.blockNum, ms)
                    )
            case p: Final =>
                List(
                  ResolvedEffect
                      .Tx(
                        p.finalization.tx.id,
                        opener,
                        EffectKind.Finalization,
                        p.finalization.tx,
                        None
                      )
                )
            case p: Minor[StandaloneEvacuationCommitment.MultiSigned] =>
                List(
                  ResolvedEffect
                      .Sec(EffectIds.secL1TxId(p.sec.commitment), p.sec.commitment.blockNum, p.sec)
                )

    /** The settlement of a major partition — the effect that absorbs its deposits — if any. */
    private def settlementEffect(
        opener: BlockNumber,
        pe: PartitionEffects[StandaloneEvacuationCommitment.MultiSigned]
    ): Option[ResolvedEffect] =
        pe match
            case p: Major[StandaloneEvacuationCommitment.MultiSigned] =>
                Some(
                  ResolvedEffect
                      .Tx(p.settlement.tx.id, opener, EffectKind.Settlement, p.settlement.tx, None)
                )
            case _ => None

    /** A deposit's post-dated refund in `pe`, matched by the `requestId` it carries; attributed to
      * the deposit's registration block.
      */
    private def depositRefundEffect(
        requestId: RequestId,
        block: BlockNumber,
        pe: PartitionEffects[StandaloneEvacuationCommitment.MultiSigned]
    ): Option[ResolvedEffect] =
        val refunds: List[RefundTx] = pe match
            case p: Major[StandaloneEvacuationCommitment.MultiSigned] => p.refunds
            case p: Minor[StandaloneEvacuationCommitment.MultiSigned] => p.refunds
            case _: Final                                             => Nil
        refunds.collectFirst {
            case r: RefundTx.PostDated if r.requestId == requestId =>
                ResolvedEffect.Tx(r.tx.id, block, EffectKind.Refund, r.tx, None)
        }

    /** The stack's block briefs in block order (its `[firstBlockNum, lastBlockNum]` range). */
    private def stackBriefs(stackNum: StackNumber): IO[List[BlockBrief.Next]] =
        reader.stackBrief(stackNum).flatMap {
            case None => IO.pure(Nil)
            case Some(brief) =>
                ((brief.firstBlockNum: Int) to (brief.lastBlockNum: Int)).toList
                    .traverse(n => reader.blockBrief(BlockNumber(n)))
                    .map(_.flatten)
        }

    /** Group a stack's blocks into its partitions' block-number lists — mirrors
      * [[hydrozoa.multisig.ledger.stack.StackPartition.partition]] exactly (leading minor run;
      * Major + trailing same-nothing minors; Final alone), so the groups align 1:1 with the
      * persisted [[StackEffects.HardConfirmed.Regular.partitions]].
      */
    private def partitionGroups(briefs: List[BlockBrief.Next]): List[NonEmptyList[BlockNumber]] =
        def isMinor(b: BlockBrief.Next): Boolean = b match
            case _: BlockBrief.Minor => true
            case _                   => false

        @annotation.tailrec
        def loop(
            remaining: List[BlockBrief.Next],
            acc: List[NonEmptyList[BlockNumber]]
        ): List[NonEmptyList[BlockNumber]] =
            remaining match
                case Nil => acc.reverse
                case head :: rest =>
                    head match
                        case _: BlockBrief.Minor =>
                            val (minors, rest2) = remaining.span(isMinor)
                            loop(rest2, NonEmptyList.fromListUnsafe(minors.map(_.blockNum)) :: acc)
                        case _: BlockBrief.Major =>
                            val (trailing, rest2) = rest.span(isMinor)
                            loop(
                              rest2,
                              NonEmptyList(head.blockNum, trailing.map(_.blockNum)) :: acc
                            )
                        case _ =>
                            (NonEmptyList.one(head.blockNum) :: acc).reverse

        loop(briefs, Nil)
