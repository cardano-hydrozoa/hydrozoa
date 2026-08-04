package hydrozoa.multisig.ledger.remote

import cats.data.EitherT
import cats.effect.IO
import hydrozoa.multisig.ledger.l2.{L2LedgerCommand, L2ScreenError, L2Screener}
import scalus.uplc.builtin.ByteString

/** Screening for a remote L2 ledger. A passthrough for now: a remote ledger accepts every request
  * pre-RequestId and does its own screening at submission. A dedicated remote screening endpoint
  * (docs/l2-isomorphism.md, Limitations) replaces this with a real, independently-connected
  * screener so a remote node also rejects pre-RequestId — and because screening rides its own trait
  * now, that endpoint gets its own connection without touching the mutation transport.
  */
object RemoteL2Screener extends L2Screener[IO] {
    override def screenTx(l2Payload: ByteString): EitherT[IO, L2ScreenError, Unit] =
        EitherT.rightT[IO, L2ScreenError](())

    override def screenDeposit(
        req: L2LedgerCommand.ScreenDeposit
    ): EitherT[IO, L2ScreenError, Unit] =
        EitherT.rightT[IO, L2ScreenError](())
}
