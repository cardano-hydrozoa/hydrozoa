package hydrozoa.multisig.ledger.l2

/** A ledger's state at one command number, serialized into a form another instance of the **same
  * backend** can adopt — what a hub hands a coil peer joining from a snapshot
  * (`docs/spec/coil-network.md`; GUM-312).
  *
  * `bytes` is **opaque to everyone but the backend that produced it.** `l2Ledger: L2LedgerKind` is
  * a head parameter pinned in `headParamsHash`, so every peer in one head drives the same backend
  * and an export never has to cross a backend boundary. Nothing outside an [[L2Ledger]]
  * implementation may parse, splice or inspect these bytes.
  *
  * **It is not self-attesting.** A joining peer must not trust an export because it arrived: it
  * imports it, asks its own ledger for the resulting digests, and checks those against the
  * `l2StateHash` on a certificate the head peers signed. The `commandNumber` beside the bytes says
  * which boundary the import claims to land on, and the importer reports what it actually reached —
  * a claim to check, not a fact to adopt.
  */
final case class L2StateExport(
    commandNumber: L2CommandNumber,
    bytes: IArray[Byte]
) {

    /** Size of the serialized state, for logging and for bounding a transfer. */
    def sizeBytes: Int = bytes.length
}
