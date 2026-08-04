package hydrozoa.multisig.ledger.eutxol2

import cats.data.EitherT
import cats.effect.IO
import hydrozoa.multisig.ledger.eutxol2.tx.{L2Genesis, L2Tx}
import hydrozoa.multisig.ledger.l2.{L2LedgerCommand, L2ScreenError, L2Screener}
import scala.util.Try
import scalus.uplc.builtin.ByteString

/** Stateless EUTXO screening (docs/l2-isomorphism.md), split from [[EutxoL2Ledger]] so it shares no
  * mutable state with the state-mutating command stream and can ride its own connection — mirroring
  * the remote side ([[hydrozoa.multisig.ledger.remote.RemoteL2Screener]]). Needs only `config`; the
  * two deposit gates it shares with the command path live in [[EutxoDepositGates]].
  */
final class EutxoL2Screener(config: EutxoL2Ledger.Config) extends L2Screener[IO]:

    override def screenTx(l2Payload: ByteString): EitherT[IO, L2ScreenError, Unit] =
        // The native L2 tx must parse, carry this head's headId pin, and have valid vkey-witness
        // signatures over the tx id. All stateless; the stateful required-signers / balance / input
        // checks stay at submission (an unsigned tx that slips past here is still rejected there by
        // MissingKeyHashes).
        EitherT.fromEither[IO](for {
            l2Tx <- L2Tx.parse(l2Payload.bytes, config).left.map(L2ScreenError(_))
            _ <- HeadIdPinValidator.validate(config, l2Tx.tx).left.map(L2ScreenError(_))
            _ <- HydrozoaTransactionMutator
                .screenSignatures(config, l2Tx)
                .left
                .map(e => L2ScreenError(e.toString))
        } yield ())

    override def screenDeposit(
        req: L2LedgerCommand.ScreenDeposit
    ): EitherT[IO, L2ScreenError, Unit] =
        EitherT.fromEither[IO](for {
            // The l2Payload must decode to the deposit's GenesisObligations — the utxos this ledger
            // will spawn when the deposit is absorbed. Shares the decode with registration
            // (fromDepositPayload), so nothing screened here can fail to register or absorb later.
            l2Genesis <- Try(
              L2Genesis.fromDepositPayload(req.depositId, req.l2Payload)
            ).toEither.left.map(e => L2ScreenError(s"Invalid deposit transaction payload $e"))
            // depositL2Value must cover the spawned outputs, and each output must clear min-ada on
            // its own — the same two gates registration applies, so a screened deposit can be both
            // registered and absorbed.
            _ <- EutxoDepositGates
                .validateDepositCover(l2Genesis, req.depositL2Value)
                .left
                .map(L2ScreenError(_))
            _ <- EutxoDepositGates
                .validateSpawnedOutputs(l2Genesis, config)
                .left
                .map(L2ScreenError(_))
        } yield ())
