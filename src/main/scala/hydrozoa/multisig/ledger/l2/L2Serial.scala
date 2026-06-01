package hydrozoa.multisig.ledger.l2

import io.circe.{Codec, Decoder, Encoder}

/** A monotonic commit counter internal to an [[L2Ledger]] — the recovery anchor for L2 state.
  *
  * The L2 ledger is a black box that knows nothing of acks, blocks, stacks, or confirmations, so
  * its persisted snapshots / command log are keyed by *this* (its own commit count), never by a
  * consensus marker. It increments by one on each successful state-mutating command (the "real"
  * commands — deposit registration, deposit decisions, transaction application — not the transient
  * proxy commands). Recovery addresses a committed L2 state purely by serial:
  * [[L2Ledger.restoreTo]] reconstructs the state as of a given serial from
  * `(initial state, serial)`.
  *
  * See `design/recovery-implementation-plan.md` R2b.
  */
type L2Serial = L2Serial.L2Serial

object L2Serial:
    opaque type L2Serial = Long

    /** The serial of a freshly-initialized ledger (the genesis state, before any commit). */
    val zero: L2Serial = 0L

    def apply(n: Long): L2Serial =
        require(n >= 0, s"L2Serial must be non-negative, got: $n")
        n

    given Codec[L2Serial] = Codec.from(Decoder.decodeLong, Encoder.encodeLong)

    given Ordering[L2Serial] = Ordering.Long

    given Conversion[L2Serial, Long] = identity

    extension (self: L2Serial)
        /** The next serial — one commit later. */
        def increment: L2Serial = self + 1L

        /** The underlying `Long` (for arithmetic such as the snapshot-interval modulo). */
        def value: Long = self
