package hydrozoa.multisig.ledger.eutxol2

import hydrozoa.config.head.initialization.InitializationParameters.HeadId
import hydrozoa.config.head.initialization.InitializationParameters.HeadId.toHex
import scalus.cardano.ledger.AuxiliaryData.Metadata
import scalus.cardano.ledger.{Metadatum, Transaction, Word64}

/** The headId pin: every L2 transaction carries the head's `headId` in a dedicated transaction
  * metadatum so the ledger can reject cross-head replay (docs/l2-isomorphism.md). The value follows
  * the L1 convention (`Metadatum.Text(headId.toHex)`); the label is **distinct** from the CIP-67
  * head tag (4937), which carries the per-output L1/L2 designation list.
  */
object HeadIdPin {

    /** The transaction-metadata label carrying the headId pin (distinct from the CIP-67 head tag).
      */
    val metadataLabel: Long = 4936

    /** The metadatum entry a pin-valid L2 tx must carry. */
    def metadatum(headId: HeadId): (Word64, Metadatum) =
        Word64(metadataLabel) -> Metadatum.Text(headId.toHex)

    /** The headId pin (hex) carried by `tx`, if present and well-formed. */
    def extract(tx: Transaction): Option[String] =
        tx.auxiliaryData.flatMap(_.value match {
            case Metadata(m) => m.get(Word64(metadataLabel)).collect { case Metadatum.Text(h) => h }
            case _           => None
        })
}

/** Rejects an L2 tx whose `headId` pin does not match this head's configured `headId` — cross-head
  * replay protection. Skipped when the head runs in identity-isomorphism mode (`identityIsomorphism =
  * true`), where the exact L1 tx runs unchanged and carries no pin.
  */
object HeadIdPinValidator {
    def validate(config: EutxoL2Ledger.Config, tx: Transaction): Either[String, Unit] =
        checkPin(config.identityIsomorphism, config.headId.toHex, HeadIdPin.extract(tx))

    /** The pin decision, over primitives so it is testable without a full transaction: skip when
      * `identityIsomorphism`; otherwise the extracted pin must equal `expectedHeadIdHex`.
      */
    def checkPin(
        identityIsomorphism: Boolean,
        expectedHeadIdHex: String,
        pin: Option[String]
    ): Either[String, Unit] =
        if identityIsomorphism then Right(())
        else
            pin match {
                case Some(h) if h == expectedHeadIdHex => Right(())
                case Some(h) =>
                    Left(s"headId pin mismatch: L2 tx carries $h, expected $expectedHeadIdHex")
                case None =>
                    Left("headId pin: L2 tx carries no headId metadatum")
            }
}
