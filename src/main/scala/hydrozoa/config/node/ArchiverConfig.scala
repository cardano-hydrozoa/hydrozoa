package hydrozoa.config.node

import hydrozoa.lib.cardano.scalus.QuantizedTime.given
import io.circe.*
import io.circe.generic.semiauto.*
import scala.concurrent.duration.{DurationInt, FiniteDuration}

/** Declares that an archiver is attached to this node's store.
  *
  * The node cannot find this out for itself, and that is structural rather than a gap. An archiver
  * reads the store as a RocksDB secondary and is never dialed, so the only thing the node ever
  * hears from one is a watermark saying how far it durably copied. The **absence** of that
  * watermark is ambiguous between two situations that call for opposite behaviour:
  *
  *   - no archiver exists — delete as soon as consensus allows;
  *   - an archiver exists but is down, or has not reported yet — delete nothing.
  *
  * Nothing the node can measure separates those, so which one holds has to be declared. Letting an
  * archiver announce itself instead would not do: a node restart forgets the announcement, and in
  * the window before the archiver reconnects the node would conclude "no archiver" and delete data
  * the archive never copied — the unsafe reading, on every restart. Registration can say an
  * archiver is *alive*; only configuration can say one is *expected*.
  *
  * The two signals therefore answer different questions and neither substitutes for the other: this
  * config says whether there should be an archiver, and the watermark says how far it got and
  * whether it is still running.
  *
  * **Node-local, and deliberately outside `headParamsHash`.** Whether this peer runs an archiver
  * bounds what it does to its own disk; no follower's validation depends on it, which is the test
  * `docs/spec/head-params-hash.md` applies to `rateLimits` for the same reason. Covering it would
  * make attaching an archiver a head re-initialization, and would force every peer to agree on a
  * deployment choice that is one operator's business.
  */
final case class ArchiverConfig(
    /** How long a watermark may go unrefreshed before the archiver counts as not running.
      *
      * Past it, retention stalls rather than falling back to the no-archiver rule: a silent
      * archiver and an absent one are the same ambiguity as above, and resolving it the convenient
      * way is what would delete unarchived data. It is an alert condition, not a licence.
      *
      * The default allows for the archiver's own cadence — it tails journals every few seconds but
      * rescans the non-journal families on a much slower pass — so a node does not report a stale
      * archive merely because a full pass is in flight.
      */
    staleAfter: FiniteDuration = 15.minutes
)

object ArchiverConfig {
    given archiverConfigEncoder: Encoder[ArchiverConfig] = deriveEncoder[ArchiverConfig]

    /** Decoded leniently in one respect: an empty object means "an archiver is attached, with the
      * default staleness window". Declaring one should not require knowing the knob exists.
      */
    given archiverConfigDecoder: Decoder[ArchiverConfig] =
        Decoder.instance(c =>
            c.downField("staleAfter")
                .as[Option[FiniteDuration]]
                .map(staleAfter =>
                    ArchiverConfig(staleAfter.getOrElse(ArchiverConfig().staleAfter))
                )
        )
}
