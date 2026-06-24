package hydrozoa.multisig.persistence.recovery

/** Merge the per-journal tails into one totally-ordered stream by [[ArrivalStamp]] — the §5.4
  * interleaving the `ReplayActor` (R3) and the one-by-one oracle (R4) both consume.
  *
  * **Why a total gather-and-sort.** The per-journal streams are in fact already sorted by stamp: an
  * inbound entry is persisted in receive-cursor order, which is gap-free and next-expected (CR8),
  * so within a journal stamps are non-decreasing in index; own entries are created in index order;
  * and since block / stack production is sequential (entry `i+1` depends on `i`), own-creation and
  * remote-receipt stamps interleave monotonically on the spines. So a k-way merge across the
  * per-journal scans would also be correct. We gather-and-sort anyway because it is simpler and the
  * recovery tail is bounded (the acked-but-unconfirmed band, within the timing budget — §1/CR7), so
  * the streaming machinery buys nothing; R3 can swap in a true k-way merge if a profile ever shows
  * it matters.
  *
  * **Order is total and deterministic.** `monotonicNanos` comes from a single `IO.monotonic` source
  * and `generation` separates processes (§5.4), so the stamp orders almost everything on its own —
  * but ties are **normal, not degenerate**: `PeerLiaisonHeadToHead` assigns one arrival stamp per
  * inbound batch, so every entry delivered in the same batch (a block, an ack, several requests)
  * shares it. The `(cf-ordinal, key-bytes)` tiebreak is therefore load-bearing — within a journal
  * it reduces to index order; across journals it fixes a deterministic interleaving of same-batch
  * entries. Consensus is interleaving-robust (§5.6), so any deterministic choice within a tie is
  * valid; the tiebreak only has to make the merge a pure function of its input.
  *
  * See `design/persistence-and-crash-recovery.md` §5.4 and `design/recovery-implementation-plan.md`
  * R1.
  */
object ArrivalOrderedMerge:

    /** Merge already-per-journal-grouped tails (e.g. from [[JournalScan.scanJournals]]) into one
      * stamp-ordered stream.
      */
    def merge(perJournal: List[List[RawJournalEntry]]): List[RawJournalEntry] =
        mergeAll(perJournal.flatten)

    /** Order a flat bag of entries by arrival stamp (then the deterministic tiebreak). */
    def mergeAll(entries: List[RawJournalEntry]): List[RawJournalEntry] =
        entries.sorted(entryOrdering)

    /** Unsigned lexicographic order on raw key bytes — byte-for-byte, low index first, comparing
      * each byte as an **unsigned** value in `[0, 255]`; if one key is a prefix of the other, the
      * shorter sorts first.
      *
      * **Why unsigned, and why by hand.** The JVM `Byte` is *signed* (`[-128, 127]`), so a naive
      * `a(i) - b(i)` would sort `0x80` *before* `0x7f` — the opposite of numeric order. The journal
      * keys (§7.1) are big-endian, fixed-width integers whose high bytes routinely exceed `0x7f`
      * (e.g. `HeadPeerNumber` 200 = `0xC8`, or any `blockNum` past `0x80000000`), so a signed
      * compare would scramble exactly the keys this tiebreak exists to order. Masking with `& 0xff`
      * widens each byte to its unsigned `Int` value before subtracting, giving true
      * unsigned-lexicographic order. `Array[Byte]` also has no usable structural `Ordering` out of
      * the box, so this is spelled out rather than summoned.
      *
      * **Why it must match the store.** This *is* the order the backends use for keys: RocksDB's
      * default comparator is unsigned-bytewise, and `InMemoryBackendStore` mirrors it with
      * `(b & 0xff)`. Because the keys are big-endian fixed-width, that byte order coincides with
      * numeric index order — so within one journal this tiebreak degrades to ordinary index order,
      * and across journals (already split by `cf.ordinal` above) it never actually runs on mixed
      * widths. Keeping it identical to the store's comparator means the recovery merge can never
      * disagree with on-disk key order. (The §5.4 stamp is the primary key; this only breaks
      * same-batch stamp ties — see [[ArrivalOrderedMerge]].)
      *
      * The final `a.length - b.length` is the prefix rule: equal on the shared prefix ⇒ shorter
      * first. It does not arise for same-CF keys (fixed width) but keeps the ordering total for any
      * input.
      */
    private val unsignedBytesOrdering: Ordering[Array[Byte]] = (a, b) =>
        val limit = math.min(a.length, b.length)
        var i = 0
        var cmp = 0
        while i < limit && cmp == 0 do
            cmp = (a(i) & 0xff) - (b(i) & 0xff)
            i += 1
        if cmp != 0 then math.signum(cmp) else a.length - b.length

    /** `(generation, monotonicNanos, cf-name)` then unsigned key bytes — see
      * [[ArrivalOrderedMerge]]. The CF name is the stable per-CF tiebreaker (the per-author split
      * made `Cf` a sealed trait without an enum `ordinal`, §7.1).
      */
    private val entryOrdering: Ordering[RawJournalEntry] =
        Ordering
            .by[RawJournalEntry, (Int, Long, String)](e =>
                (e.stamp.generation, e.stamp.monotonicNanos, e.key.cf.name)
            )
            .orElse(Ordering.by[RawJournalEntry, Array[Byte]](_.key.encode)(unsignedBytesOrdering))
