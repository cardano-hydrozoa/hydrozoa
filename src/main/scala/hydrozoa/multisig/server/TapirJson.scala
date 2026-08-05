package hydrozoa.multisig.server

import io.circe.Printer
import sttp.tapir.json.circe.TapirJsonCirce

/** tapir's circe `jsonBody`, but serializing with null values dropped.
  *
  * The API DTOs use `Option` fields for genuinely-absent data (e.g. an output's datum). tapir
  * derives their OpenAPI schema with those fields simply *not required*; circe's default printer,
  * however, emits an explicit `null` for a `None`. That mismatch means an absent optional would be
  * sent as `"field": null`, which does not validate against the "not required" schema. Dropping
  * nulls omits the key instead, so the wire JSON matches the generated schema.
  */
object TapirJson extends TapirJsonCirce {
    override def jsonPrinter: Printer = Printer.noSpaces.copy(dropNullValues = true)
}
