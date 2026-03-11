package hydrozoa.multisig.ledger.eutxol2

import cats.*
import cats.data.*
import cats.effect.{Async, IO, Ref}
import cats.syntax.all.*
import hydrozoa.config.head.initialization.InitializationParameters
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.multisig.ledger.eutxol2.tx.{L2Genesis, L2Tx}
import hydrozoa.multisig.ledger.event.RequestId
import hydrozoa.multisig.ledger.joint.obligation.Payout
import hydrozoa.multisig.ledger.joint.{EvacuationDiff, EvacuationKey}
import hydrozoa.multisig.ledger.l2.*
import hydrozoa.multisig.ledger.l2.L2LedgerCommand.RegisterDepositRequest
import io.bullet.borer.Cbor
import monocle.syntax.all.*
import scala.util.Try
import scalus.cardano.ledger.*

extension (ti: TransactionInput) {
    // Technically, this is partial -- but with the current cbor codec of TransactionInput
    // and invariants enforced on EvacuationKey.apply (36 bytes), this should not throw
    def toEvacuationKey: EvacuationKey = EvacuationKey(Cbor.encode(ti).toByteArray).get
}

object EutxoL2Ledger {
    type Config = CardanoNetwork.Section & InitializationParameters.Section

    case class State(activeUtxos: Utxos, pendingDeposits: Map[RequestId, L2Genesis])

    // As above: technically partial, but used in the context of the EutxoL2Ledger, it's not.
    private def toTransactionInput(ek: EvacuationKey): TransactionInput =
        Cbor.decode(ek.bytes).to[TransactionInput].value

    def apply(
        config: EutxoL2Ledger.Config,
    ): IO[EutxoL2Ledger] = {

        for {
            ref <- Ref[IO].of(
              State(
                activeUtxos = config.initialEvacuationMap.cooked.map((ti, to) =>
                    toTransactionInput(ti) -> to
                ),
                pendingDeposits = Map.empty
              )
            )
        } yield new EutxoL2Ledger(config, ref)
    }
}

case class EutxoL2Ledger private (
    config: EutxoL2Ledger.Config,
    private val state: Ref[IO, EutxoL2Ledger.State]
) extends L2Ledger[IO] {
    implicit def monadF: Monad[IO] = Async[IO]

    override def sendL2Event(
        req: L2LedgerCommand.ApplyTransactionRequest
    ): EitherT[IO, L2LedgerError, (Vector[EvacuationDiff], Vector[Payout.Obligation])] = {
        for {
            s <- EitherT.right(state.get)
            l2Tx <- EitherT.fromEither(
              L2Tx.parse(req.l2Payload)
                  .left
                  .map(error =>
                      L2LedgerError(
                        error.getBytes
                      )
                  )
            )

            newActiveUtxos <- EitherT.fromEither(
              HydrozoaTransactionMutator
                  .transit(
                    config = config,
                    time = req.blockCreationStartTime,
                    state = s.activeUtxos,
                    l2Tx = l2Tx
                  )
                  .left
                  .map(error => L2LedgerError(error.toString.getBytes))
            )

            adds =
                newActiveUtxos
                    .removedAll(s.activeUtxos.keys)
                    .map((ti, to) => EvacuationDiff.Update(ti.toEvacuationKey, KeepRaw(to)))
                    .toVector

            deletes =
                s.activeUtxos
                    .removedAll(newActiveUtxos.keys)
                    .map((ti, _) => EvacuationDiff.Delete(ti.toEvacuationKey))
                    .toVector

            _ <- EitherT.right(state.set(s.focus(_.activeUtxos).replace(newActiveUtxos)))
        } yield (
          adds ++ deletes,
          Vector.from(l2Tx.l1utxos.map((_, to) => Payout.Obligation(KeepRaw(to))))
        )
    }

    override def sendDepositEventRegistration(
        req: RegisterDepositRequest
    ): EitherT[IO, L2LedgerError, Unit] =
        for {
            s <- EitherT.right(state.get)
            l2Genesis <-
                EitherT.fromEither(
                  Try(L2Genesis.fromDepositEventRegistration(req)).toEither.left
                      .map(e => L2LedgerError(s"Invalid deposit transaction payload $e".getBytes))
                )
            newState = s
                .focus(_.pendingDeposits)
                .modify(pending => pending.updated(req.requestId, l2Genesis))
            _ <- EitherT.right(state.set(newState))
        } yield ()

    override def sendDepositEventDecisions(
        req: L2LedgerCommand.ApplyDepositDecisions
    ): EitherT[IO, L2LedgerError, Vector[EvacuationDiff]] =
        for {
            s <- EitherT.right(state.get)
            addedL2Utxos = req.absorbedDeposits.flatMap(id => s.pendingDeposits(id).asUtxos)
            newState =
                s
                    .focus(_.activeUtxos)
                    .modify(_ ++ addedL2Utxos.map((i, o) => i -> o.value))
                    .focus(_.pendingDeposits)
                    .modify(_.removedAll(req.absorbedDeposits ++ req.rejectedDeposits))
            _ <- EitherT.right(state.set(newState))
            evacuationDiffs: Vector[EvacuationDiff] =
                Vector.from(addedL2Utxos.map((i, o) => EvacuationDiff.Update(i.toEvacuationKey, o)))
        } yield evacuationDiffs
}
