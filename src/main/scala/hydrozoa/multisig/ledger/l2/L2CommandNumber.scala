package hydrozoa.multisig.ledger.l2

import io.circe.{Codec, Decoder, Encoder}

/** A monotonic command number for an [[L2Ledger]] — the recovery anchor for L2 state.
  *
  * The L2 ledger is a black box that knows nothing of acks, blocks, stacks, or confirmations, so
  * its persisted snapshots / command log are keyed by *this*, never by a consensus marker.
  * JointLedger assigns each command (deposit registration, deposit decisions, transaction
  * application) the next number and the ledger validates and adopts it — one number per command
  * evaluated (applied *or* rejected), in lock-step across every peer's replica. Recovery addresses
  * a committed L2 state purely by command number: [[L2Ledger.restoreTo]] reconstructs the state as
  * of a given command number from `(initial state, command number)`.
  *
  * See `docs/l2-ledger-command-coordination.md`.
  */
type L2CommandNumber = L2CommandNumber.L2CommandNumber

object L2CommandNumber:
    opaque type L2CommandNumber = Long

    /** The command number of a freshly-initialized ledger (the genesis state, before any commit).
      */
    val zero: L2CommandNumber = 0L

    def apply(n: Long): L2CommandNumber =
        require(n >= 0, s"L2CommandNumber must be non-negative, got: $n")
        n

    given Codec[L2CommandNumber] = Codec.from(Decoder.decodeLong, Encoder.encodeLong)

    given Ordering[L2CommandNumber] = Ordering.Long

    given Conversion[L2CommandNumber, Long] = identity

    extension (self: L2CommandNumber)
        /** The next command number — one commit later. */
        def increment: L2CommandNumber = self + 1L

        /** The underlying `Long` (for arithmetic such as the snapshot-interval modulo). */
        def value: Long = self
