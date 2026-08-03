package hydrozoa.rulebased.ledger.l1

import scalus.cardano.ledger.DatumOption
import scalus.cardano.ledger.DatumOption.Inline
import scalus.uplc.builtin.Data.toData
import scalus.uplc.builtin.{ByteString, Data}

/** Content-based sentinel datums used by the test-only `RBRClassifier` to bucket L1 UTxOs by role.
  *
  * Writers (test generators / fixtures) plant these on outputs at construction time; the reader
  * (`RBRClassifier`) matches on the raw [[Data]] marker to identify the role. Both sides must agree
  * exactly on the sentinel bytes, so callers route through these helpers rather than reconstructing
  * the `ByteString.fromString(...)` literal at each site.
  */
object RbrDatumSentinels:

    /** Build the raw sentinel marker from its ASCII label. */
    def marker(name: String): Data = toData(ByteString.fromString(name))

    /** Build an inline `datumOption` carrying [[marker]]`(name)`. */
    def inline(name: String): Option[DatumOption] = Some(Inline(marker(name)))
