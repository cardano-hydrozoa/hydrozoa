package hydrozoa.multisig.ledger.eutxol2

import hydrozoa.config.head.initialization.InitializationParameters.HeadId
import hydrozoa.config.head.initialization.InitializationParameters.HeadId.toHex

/** Rejects an L2 tx whose headId pin does not match this head's configured `headId` — cross-head
  * replay protection. The pin is the headId key carried by the L2 head-label metadata
  * ([[hydrozoa.multisig.ledger.eutxol2.tx.L2Metadata]]); it is authenticated for free via the tx's
  * `auxiliaryDataHash`. Skipped when the head runs in identity-isomorphism mode
  * (`identityIsomorphism = true`), where the exact L1 tx runs unchanged and carries no pin.
  */
object HeadIdPinValidator {
    def validate(config: EutxoL2Ledger.Config, headId: HeadId): Either[String, Unit] =
        checkPin(config.identityIsomorphism, config.headId.toHex, headId.toHex)

    /** The pin decision, over primitives so it is testable without a full transaction: skip when
      * `identityIsomorphism`; otherwise the tx's headId must equal `expectedHeadIdHex`.
      */
    def checkPin(
        identityIsomorphism: Boolean,
        expectedHeadIdHex: String,
        actualHeadIdHex: String
    ): Either[String, Unit] =
        if identityIsomorphism then Right(())
        else if actualHeadIdHex == expectedHeadIdHex then Right(())
        else
            Left(
              s"headId pin mismatch: L2 tx carries $actualHeadIdHex, expected $expectedHeadIdHex"
            )
}
