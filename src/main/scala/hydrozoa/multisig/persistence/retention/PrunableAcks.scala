package hydrozoa.multisig.persistence.retention

import cats.effect.IO
import cats.syntax.traverse.*
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.multisig.consensus.ack.{HardAckNumber, SoftAckNumber}
import hydrozoa.multisig.consensus.peer.{HeadPeerNumber, PeerId}
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.stack.StackNumber
import hydrozoa.multisig.persistence.{AckRetention, BackendStore, Cf, JournalKey}

/** The ack keys a confirmation has made redundant and retention allows deleting.
  *
  * Byte-level reads, like [[hydrozoa.multisig.persistence.Markers]] — the access `BackendStore`
  * reserves for "specialised modules that derive state from raw key scans". This module only
  * *finds* keys; the caller writes the batch, so the decision to delete stays in one place.
  *
  * Both scans **stop at the bound rather than reading the journal**. That matters: a journal can
  * hold millions of entries, and a scan that materialises all of them before filtering is the same
  * shape as the unauthenticated OOM on the query endpoints. Here the work is proportional to what
  * is being deleted, not to what is retained.
  */
object PrunableAcks:

    /** Soft-ack keys at or below `upToBlock`, across every head peer's journal.
      *
      * A soft-ack's number **is** its block number — `SoftAck` builds its id as
      * `SoftAckNumber(blockNum)` so each peer's sequence stays gap-free — so the walk needs no
      * payload decode: the key alone says which block an entry belongs to.
      *
      * No marker reads a `SoftAck` family, so nothing is held back; every entry at or below the
      * bound is a candidate. Every head peer's journal is swept, not just this node's own: a
      * follower stores its peers' acks here too, and one `SoftConfirmation` subsumes all of them.
      */
    def softAcks(
        backend: BackendStore[IO],
        peers: List[HeadPeerNumber],
        upToBlock: BlockNumber,
        retention: AckRetention
    ): IO[List[JournalKey]] =
        peers.flatTraverse { peer =>
            val cf = Cf.SoftAck(peer)
            walkWhile(backend, cf, JournalKey.SoftAck(peer, SoftAckNumber.zero)) {
                case (key: JournalKey.SoftAck, _) => (key.num: Int) <= (upToBlock: Int)
                case _                            => false
            }.map(_.collect {
                case key: JournalKey.SoftAck if retention.mayPrune(cf, (key.num: Int).toLong) =>
                    key
            })
        }

    /** Hard-ack keys for stacks at or below `upToStack`, across every author's journal.
      *
      * A hard-ack's number is an independent per-peer cursor advancing once per
      * `(stackNum, round)`, so unlike a soft-ack it cannot be computed from the stack number. It
      * does not need to be — **the ack's own value carries `stackNum`** — so the walk decodes each
      * entry and stops at the first stack above the bound. A peer emits its rounds for one stack
      * before moving to the next, so `stackNum` rises with `hardAckNum` and that first miss really
      * is the end.
      *
      * `protectedFamilies` names the journals whose highest key is a recovery marker
      * ([[hydrozoa.multisig.persistence.Markers.markerFamilies]]). Their last entry is withheld
      * even when it is otherwise prunable: emptying such a family does not lose history, it makes
      * the next boot read the store as cold.
      */
    def hardAcks(
        backend: BackendStore[IO],
        authors: List[PeerId],
        upToStack: StackNumber,
        retention: AckRetention,
        protectedFamilies: Set[Cf]
    )(using CardanoNetwork.Section): IO[List[JournalKey]] =
        authors.flatTraverse { peer =>
            val cf = Cf.HardAck(peer)
            val decodeStack: Array[Byte] => StackNumber =
                bytes =>
                    JournalKey.HardAck(peer, HardAckNumber.zero).decodeValue(bytes).payload.stackNum
            for {
                candidates <- walkWhile(backend, cf, JournalKey.HardAck(peer, HardAckNumber.zero)) {
                    (_, value) => (decodeStack(value): Int) <= (upToStack: Int)
                }
                // Withheld by key, not by position in `candidates`: the family's highest key may
                // sit above the bound and never be a candidate at all, in which case nothing needs
                // holding back.
                withheld <-
                    if protectedFamilies.contains(cf) then backend.lastKey(cf).map(_.map(_.toSeq))
                    else IO.pure(None)
            } yield candidates.collect {
                case key: JournalKey.HardAck
                    if retention.mayPrune(cf, (key.num: Int).toLong)
                        && !withheld.contains(key.encode.toSeq) =>
                    key
            }
        }

    /** Walk `cf` from `from`, collecting keys while `take` holds and stopping at the first miss.
      *
      * The early stop is the point. Both journals are ordered by an index that rises with the thing
      * being bounded, so the first entry past the bound means every later one is too — and the read
      * costs what is deleted rather than what is kept.
      */
    private def walkWhile(
        backend: BackendStore[IO],
        cf: Cf,
        from: JournalKey
    )(take: (JournalKey, Array[Byte]) => Boolean): IO[List[JournalKey]] =
        backend.cursor(cf, from.encode).use { cursor =>
            def loop(acc: List[JournalKey]): IO[List[JournalKey]] =
                cursor.next.flatMap {
                    case None => IO.pure(acc.reverse)
                    case Some((keyBytes, valueBytes)) =>
                        val key = JournalKey.decode(cf, keyBytes)
                        if take(key, valueBytes) then loop(key :: acc)
                        else IO.pure(acc.reverse)
                }
            loop(Nil)
        }
