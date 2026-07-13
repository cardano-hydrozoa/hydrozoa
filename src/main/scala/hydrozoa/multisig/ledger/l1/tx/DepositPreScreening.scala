package hydrozoa.multisig.ledger.l1.tx

import hydrozoa.config.head.initialization.{InitialBlock, InitializationParameters}
import hydrozoa.config.head.multisig.fallback.FallbackContingency
import hydrozoa.config.head.multisig.timing.TxTiming
import hydrozoa.config.head.multisig.timing.TxTiming.RequestTimes.RequestValidityEndTime
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.multisig.ledger.l1.utxo.DepositUtxo
import hydrozoa.multisig.ledger.l2.{Destination, L2LedgerCommand}
import scala.language.implicitConversions
import scala.math.Ordering.Implicits.infixOrderingOps
import scalus.uplc.builtin.ByteString

/** Deposit pre-screening — Hydrozoa's stage of deposit screening (design note §5.5), run before the
  * ledger's [[hydrozoa.multisig.ledger.l2.L2Ledger.sendScreenDeposit]]. A deposit is not an L2 tx
  * and cannot self-authenticate, so unlike transactions (which have no pre-screening stage) it is
  * authenticated and time-gated here:
  *
  *   - the deposit tx must parse — [[DepositTx.Parse]] establishes well-formedness and verifies the
  *     depositor's endorsement of the `l2Payload` carried in the tx metadata;
  *   - the deposit must not be past its accept-by deadline (`now < validityEnd`, with `validityEnd`
  *     derived from the deposit tx's TTL, §5.3). The block-time accept-by check at application
  *     (`JointLedger.registerDeposit`) still runs; this is the earlier, wall-clock gate.
  *
  * On success it yields the [[L2LedgerCommand.ScreenDeposit]] reference data the ledger's screening
  * stage consumes.
  */
object DepositPreScreening {

    /** The sections [[DepositTx.Parse]] needs, plus nothing else. */
    type Config = CardanoNetwork.Section & HeadPeers.Section & InitialBlock.Section &
        TxTiming.Section & InitializationParameters.Section & FallbackContingency.Section

    enum Error {
        case DepositTxParseFailed(e: DepositTx.Parse.Error)
        case DepositExpired(validityEnd: RequestValidityEndTime, now: QuantizedInstant)

        override def toString: String = this match {
            case DepositTxParseFailed(e) => s"DepositTxParseFailed: $e"
            case DepositExpired(validityEnd, now) =>
                s"DepositExpired: accept-by deadline ${validityEnd: QuantizedInstant} has passed (now $now)"
        }
    }

    /** Pre-screen a deposit request: parse + authenticate the deposit tx, gate on the accept-by
      * deadline, and derive the ledger-screening reference data.
      */
    def preScreen(
        l1Payload: ByteString,
        l2Payload: ByteString,
        now: QuantizedInstant
    )(config: Config): Either[Error, L2LedgerCommand.ScreenDeposit] =
        for {
            // NB: the COSE authentication happens INSIDE the parse — DepositTx.Parse verifies that
            // the metadata's (coseKey, coseSignature) pair is a valid COSE_Sign1 and that it covers
            // blake2b_256(l2Payload), so a failed authentication surfaces as DepositTxParseFailed
            // here. TODO: Parse does far more than parsing (validation, authentication); extract
            // those steps (or rename it) so pre-screening's checks are visible at this call site.
            depositTx <- DepositTx
                .Parse(config)(l1Payload, l2Payload)
                .result
                .left
                .map(Error.DepositTxParseFailed(_))
            depositProduced = depositTx.depositProduced
            _ <- Either.cond(
              now < (depositProduced.requestValidityEndTime: QuantizedInstant),
              (),
              Error.DepositExpired(depositProduced.requestValidityEndTime, now)
            )
            refundInstructions = DepositUtxo.Refund.Instructions(
              depositProduced.datum.refundInstructions,
              config.network,
              config.slotConfig
            )
        } yield L2LedgerCommand.ScreenDeposit(
          depositId = depositProduced.utxoId,
          depositFee = depositProduced.depositFee,
          depositL2Value = depositProduced.l2Value,
          refundDestination = Destination(refundInstructions.address, refundInstructions.mbDatum),
          l2Payload = l2Payload
        )
}
