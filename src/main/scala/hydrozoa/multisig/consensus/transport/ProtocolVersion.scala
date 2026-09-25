package hydrozoa.multisig.consensus.transport

/** The version of the peer-to-peer protocol spoken on a liaison link.
  *
  * **One version, matched exactly.** A peer whose version differs from its counterpart's refuses
  * the link at the handshake rather than joining and diverging later. There is no negotiation and
  * no compatibility window, which is what lets a single number carry the whole question: a bump is
  * answered by migrating the head, not by two versions coexisting on one link.
  *
  * Deliberately distinct from the two versions beside it, because the three fail differently:
  *
  *   - [[hydrozoa.multisig.persistence.StoreVersion]] is node-local, so peers in one head may sit
  *     on different store schemas at the same time and nothing cross-checks them.
  *   - The software version is checked by nothing at all.
  */
object ProtocolVersion:

    /** Current protocol version — **1**.
      *
      * Bump on any change to what a peer must understand to participate: the frame envelopes, the
      * batch messages they carry, or the meaning of a cursor in them. A peer that cannot be talked
      * to by an unbumped counterpart is exactly the case this number exists to catch.
      */
    val current: Int = 1

    /** Outcome of the handshake's version check. */
    enum Check:
        /** The counterpart speaks our version — proceed. */
        case Compatible

        /** The counterpart speaks something else, and the link is refused. `found` is `None` for a
          * peer whose handshake carries no version at all.
          */
        case Incompatible(found: Option[Int], expected: Int)

    /** Check a counterpart's announced version against [[current]].
      *
      * `None` — a handshake with no version field — is incompatible rather than a decode failure,
      * so the log names the real problem (a counterpart too old to announce one) instead of a
      * missing JSON key.
      */
    def check(found: Option[Int]): Check =
        if found.contains(current) then Check.Compatible
        else Check.Incompatible(found, current)

    /** Render a checked version for a log line: the announced version, or `none` when absent. */
    def describe(found: Option[Int]): String = found.fold("none")(_.toString)
