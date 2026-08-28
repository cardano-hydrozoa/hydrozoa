package hydrozoa.multisig.consensus.liaison

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import hydrozoa.multisig.consensus.liaison.LaneOutbound.*
import org.scalatest.funsuite.AnyFunSuite
import scala.collection.mutable

/** Unit tests for the outbound half ([[LaneOutbound]]) — append + reply. Items are plain `Int`s
  * that are their own item number (`numberOf = identity`), so the sequencing logic is exercised in
  * isolation.
  */
class LaneOutboundTest extends AnyFunSuite {

    // Wide enough that nothing is evicted: the tests that are not about the cap should not have to
    // think about it.
    private val noEviction = 1024

    // A lane with an empty durable backing: a reply below the in-memory floor finds nothing.
    private val emptyJournal: (Int, Int) => IO[List[Int]] = (_, _) => IO.pure(Nil)

    /** A journal holding exactly `items`, read the way `LaneOutgoingBacking` reads one, counting
      * its reads.
      *
      * The count is the control for the cap tests: each asserts that a capped lane answers what an
      * uncapped one would, which is trivially true if nothing was ever evicted and no read ever
      * happened. `reads` is what makes that failure visible.
      */
    private final class StubJournal(items: Iterable[Int]) {
        var reads: Int = 0
        val read: (Int, Int) => IO[List[Int]] = (from, limit) =>
            IO { reads += 1 } >> IO.pure(items.filter(_ >= from).take(limit).toList)
    }

    private def journalOf(items: Iterable[Int]): (Int, Int) => IO[List[Int]] =
        StubJournal(items).read

    private def contiguousFrom(
        first: Int,
        maxPerReply: Int = 1,
        outboxCap: Int = noEviction,
        serveFromJournal: (Int, Int) => IO[List[Int]] = emptyJournal
    ): LaneOutbound[Int, Int] =
        LaneOutbound.contiguous[Int, Int](
          numberOf = identity,
          first = first,
          increment = _ + 1,
          maxPerReply,
          outboxCap = outboxCap,
          serveFromJournal = serveFromJournal
        )

    // Sparse own-led schedule: the lane's zero (0) is the bootstrap sentinel, never led. This side
    // leads the even numbers >= 2.
    private def sparseOwn: LaneOutbound[Int, Int] =
        LaneOutbound.sparse[Int, Int](
          numberOf = identity,
          zero = 0,
          next = after => Some(if after % 2 == 0 then after + 2 else after + 1),
          outboxCap = noEviction,
          serveFromJournal = emptyJournal
        )

    test("contiguous append enforces gap-free order from the first number") {
        val lane = contiguousFrom(0)
        lane.append(0).unsafeRunSync()
        lane.append(1).unsafeRunSync()
        lane.append(2).unsafeRunSync()
        assert(intercept[AppendOutOfOrder](lane.append(4).unsafeRunSync()).attempted == "4")
    }

    test("contiguous append rejects a wrong first number") {
        val lane = contiguousFrom(1)
        val _ = assertThrows[AppendOutOfOrder](lane.append(0).unsafeRunSync())
        lane.append(1).unsafeRunSync() // correct first
    }

    test("reply prunes below the remote cursor and keeps the head (retransmit-safe)") {
        val lane = contiguousFrom(0)
        (0 to 3).foreach(n => lane.append(n).unsafeRunSync())
        val _ = assert(lane.reply(2).unsafeRunSync() == Items(List(2)))
        // Re-asking from the same cursor still returns the head — not dropped on send.
        val _ = assert(lane.reply(2).unsafeRunSync() == Items(List(2)))
        // Moving the cursor forward drops the old head.
        val _ = assert(lane.reply(3).unsafeRunSync() == Items(List(3)))
        assert(lane.reply(4).unsafeRunSync() == Items(Nil))
    }

    test("reply flags out-of-bounds when the remote cursor is ahead of producible") {
        val lane = contiguousFrom(0)
        lane.append(0).unsafeRunSync()
        lane.append(1).unsafeRunSync()
        // Highest appended is 1, so the remote may legitimately ask up to 2 (next-producible).
        val _ = assert(lane.reply(2).unsafeRunSync() == Items(Nil))
        // Asking past the bound is out of bounds, carrying the diagnostic indices.
        assert(
          lane.reply(3).unsafeRunSync() == OutOfBounds(asked = "3", bound = "2", lastAppended = "1")
        )
    }

    test("request-style lane batches up to maxPerReply") {
        val lane = contiguousFrom(0, maxPerReply = 2)
        (0 to 4).foreach(n => lane.append(n).unsafeRunSync())
        assert(lane.reply(1).unsafeRunSync() == Items(List(1, 2)))
    }

    test("reply reads from the store when the cursor is below the in-memory outbox floor") {
        // A stub store holding 0..4; on recovery the lane restores only its high-water (empty
        // outbox), so a pull below the in-memory floor is served from the store.
        val lane = contiguousFrom(0, maxPerReply = 2, serveFromJournal = journalOf(0 to 4))
        lane.seedHighWater(Some(4)).unsafeRunSync()
        // Outbox empty → served from the store.
        val _ = assert(lane.reply(1).unsafeRunSync() == Items(List(1, 2)))
        // A live tail item lands in memory; its own number is served from memory, older from store.
        lane.append(5).unsafeRunSync()
        val _ = assert(lane.reply(5).unsafeRunSync() == Items(List(5)))
        assert(lane.reply(2).unsafeRunSync() == Items(List(2, 3)))
    }

    test("append at or below the high-water is a no-op (absorbs a replay re-broadcast)") {
        // The store holds 0..3; on recovery the lane restores high-water = 3 (e.g. round-2), outbox
        // empty. SlowConsensusActor then re-broadcasts the in-flight stack's round-1 (= 2, below the
        // high-water) — this must be a no-op, not an out-of-order error.
        val lane = contiguousFrom(0, serveFromJournal = journalOf(0 to 3))
        lane.seedHighWater(Some(3)).unsafeRunSync()
        lane.append(2).unsafeRunSync() // re-broadcast of an already-durable ack: no throw
        // It is still servable — read from the store, since the outbox stayed empty.
        val _ = assert(lane.reply(2).unsafeRunSync() == Items(List(2)))
        // A genuinely new item (high-water + 1) still appends; a gap above it still raises.
        lane.append(4).unsafeRunSync()
        val _ = assert(lane.reply(4).unsafeRunSync() == Items(List(4)))
        assert(intercept[AppendOutOfOrder](lane.append(6).unsafeRunSync()).attempted == "6")
    }

    test("seedHighWater sets the append baseline without populating the outbox") {
        val lane = contiguousFrom(0)
        lane.seedHighWater(Some(3)).unsafeRunSync()
        // The next *new* item must continue from the high-water (4), not the cold `first`; a gap
        // above the high-water still raises (the baseline is 3, not None).
        val _ = assertThrows[AppendOutOfOrder](lane.append(5).unsafeRunSync())
        lane.append(4).unsafeRunSync()
        // Empty backing → the restored prefix is not in memory and the store has nothing to serve;
        // a pull below the tail is empty.
        assert(lane.reply(2).unsafeRunSync() == Items(Nil))
    }

    test("sparse outbound: own-led append, sentinel and remote-led numbers rejected") {
        val lane = sparseOwn
        // This side leads evens >= 2: 2, 4 ... (0 is the bootstrap sentinel, never appended).
        val _ = assertThrows[AppendOutOfOrder](lane.append(0).unsafeRunSync())
        lane.append(2).unsafeRunSync()
        lane.append(4).unsafeRunSync()
        assertThrows[AppendOutOfOrder](lane.append(5).unsafeRunSync()) // 5 is remote-led
    }

    test("a journal read never serves a store entry above the released high-water") {
        // The store durably holds 0..3, but only 0..1 are released (appended) — e.g. #2 is a
        // postponed own soft-ack the producer persisted at block time but has not yet announced
        // onto the lane. seedHighWater(1) models that: high-water 1, empty outbox, so every serve
        // reads from the store.
        val lane = contiguousFrom(0, maxPerReply = 2, serveFromJournal = journalOf(0 to 3))
        lane.seedHighWater(Some(1)).unsafeRunSync()
        // The released prefix is served from the store, capped at the high-water (no #2/#3 leak)...
        val _ = assert(lane.reply(0).unsafeRunSync() == Items(List(0, 1)))
        // ...and a pull for the not-yet-released #2 comes back empty, not served — otherwise the
        // remote's cursor would run past our bound and the next pull would be a false OutOfBounds.
        val _ = assert(lane.reply(2).unsafeRunSync() == Items(Nil))
        // Once #2 is released (announced), it serves; #3 (still unreleased) stays withheld.
        lane.append(2).unsafeRunSync()
        assert(lane.reply(2).unsafeRunSync() == Items(List(2)))
    }

    // ---- The cap ----------------------------------------------------------------------------

    test("a cap below maxPerReply is floored, so a full batch is still servable from memory") {
        // maxPerReply 4 against a cap of 1: without the floor the outbox would hold one item and
        // every pull — even from a perfectly current remote — would go to the store. The empty
        // journal makes that visible: anything not in memory comes back empty.
        val lane = contiguousFrom(0, maxPerReply = 4, outboxCap = 1)
        (0 to 3).foreach(n => lane.append(n).unsafeRunSync())
        assert(lane.reply(0).unsafeRunSync() == Items(List(0, 1, 2, 3)))
    }

    test("appending past the cap evicts the oldest, and the journal serves it") {
        val items = mutable.ArrayBuffer.empty[Int]
        val journal = StubJournal(items)
        // CR4: everything reaches the lane already durable, so the stub is written before append.
        val lane = contiguousFrom(0, outboxCap = 3, serveFromJournal = journal.read)
        (0 to 5).foreach { n =>
            items += n; lane.append(n).unsafeRunSync()
        }
        // The window is the last 3 (3..5): #4 comes from memory, so the journal is untouched...
        val _ = assert(lane.reply(4).unsafeRunSync() == Items(List(4)))
        val _ = assert(journal.reads == 0)
        // ...and #1, long evicted, comes from the journal — same answer, different source.
        val _ = assert(lane.reply(1).unsafeRunSync() == Items(List(1)))
        assert(journal.reads == 1)
    }

    test("a capped lane answers exactly as an uncapped one, over the same append/pull sequence") {
        // The property the cap has to have: it changes where an item is read from, never what the
        // remote receives. A cap of 1 is the harshest case — everything but the newest is evicted.
        val items = mutable.ArrayBuffer.empty[Int]
        val cappedJournal = StubJournal(items)
        val uncappedJournal = StubJournal(items)
        val capped =
            contiguousFrom(0, maxPerReply = 2, outboxCap = 1, serveFromJournal = cappedJournal.read)
        val uncapped = contiguousFrom(
          0,
          maxPerReply = 2,
          outboxCap = noEviction,
          serveFromJournal = uncappedJournal.read
        )

        // Three regimes, and only the first tells the two lanes apart. A remote lagging steadily
        // by 6 sits inside the uncapped window and outside the capped one, so the cap alone decides
        // where its pull is served from. Then it regresses to the start (below both floors — `reply`
        // prunes destructively, so a regressing cursor leaves an uncapped window too), and finally
        // catches up to the head (inside both).
        def cursorAt(n: Int): Int =
            if n < 10 then math.max(0, n - 6) else if n == 10 then 0 else n

        val (cappedReplies, uncappedReplies) = (0 to 15).map { n =>
            items += n
            capped.append(n).unsafeRunSync()
            uncapped.append(n).unsafeRunSync()
            val cursor = cursorAt(n)
            (capped.reply(cursor).unsafeRunSync(), uncapped.reply(cursor).unsafeRunSync())
        }.unzip

        val _ = assert(cappedReplies == uncappedReplies)
        // The control: the cap must have forced journal serves the uncapped lane did not need,
        // or the two agree only because eviction never happened.
        assert(cappedJournal.reads > uncappedJournal.reads)
    }

    test("a ceiling still bounds a reply served from the journal after eviction") {
        val items = mutable.ArrayBuffer.empty[Int]
        val journal = StubJournal(items)
        val lane =
            contiguousFrom(0, maxPerReply = 4, outboxCap = 1, serveFromJournal = journal.read)
        (0 to 9).foreach { n =>
            items += n; lane.append(n).unsafeRunSync()
        }
        // #2 is evicted, so this is a journal serve — the ceiling must apply to it too.
        val _ = assert(lane.reply(2, ceiling = Some(4)).unsafeRunSync() == Items(List(2, 3, 4)))
        assert(journal.reads == 1)
    }

    test("an evicted entry the journal cannot serve raises instead of replying empty") {
        // The failure the cap could introduce: evict an item whose backing cannot produce it, and a
        // silently empty reply leaves the remote's lane stalled forever with nothing logged. The
        // journal here is empty, so #0 is gone the moment #1 pushes it out.
        val lane = contiguousFrom(0, outboxCap = 1)
        lane.append(0).unsafeRunSync()
        lane.append(1).unsafeRunSync()
        val thrown = intercept[EvictedButUnservable](lane.reply(0).unsafeRunSync())
        val _ = assert(thrown.asked == "0")
        assert(thrown.evictedThrough == "0")
    }

    test("nothing appended yet: the lane answers without consulting the journal at all") {
        // A lane its peer never produces on — a non-hub's HubHardAck lane — is pulled on every
        // batch for the life of the process. Nothing is released, so nothing is servable whatever
        // the store holds, and asking the store would be a read per pull that is then discarded.
        // The backing here raises to prove it is not called; on a non-hub it genuinely does.
        var consulted = false
        val lane = contiguousFrom(
          0,
          serveFromJournal = (_, _) =>
              IO { consulted = true } >> IO.raiseError(IllegalStateException("no backing"))
        )
        val _ = assert(lane.reply(0).unsafeRunSync() == Items(Nil))
        assert(!consulted)
    }
}
