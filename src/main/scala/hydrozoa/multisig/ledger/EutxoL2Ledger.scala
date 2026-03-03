package hydrozoa.multisig.ledger

import cats.*
import cats.data.*
import cats.effect.{Async, IO, Ref}
import cats.syntax.all.*
import hydrozoa.config.head.initialization.InitializationParameters
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.multisig.ledger.dapp.utxo.DepositTuple
import hydrozoa.multisig.ledger.joint.obligation.Payout
import hydrozoa.multisig.ledger.virtual.*
import hydrozoa.multisig.ledger.virtual.tx.{L2Genesis, L2Tx}
import io.bullet.borer.Cbor
import scalus.cardano.ledger.*

extension (ti: TransactionInput) {
    // Technically, this is partial -- but with the current cbor codec of TransactionInput
    // and invariants enforced on EvacuationKey.apply (36 bytes), this should not throw
    def toEvacuationKey: EvacuationKey = EvacuationKey(Cbor.encode(ti).toByteArray).get
}

object EutxoL2Ledger {
    type Config = CardanoNetwork.Section & InitializationParameters.Section
    type State = Utxos

    // As above: technically partial, but used in the context of the EutxoL2Ledger, it's not.
    private def toTransactionInput(ek: EvacuationKey): TransactionInput =
        Cbor.decode(ek.bytes).to[TransactionInput].value

    def apply(
        config: EutxoL2Ledger.Config,
    ): IO[EutxoL2Ledger] = {

        for {
            ref <- Ref[IO].of(
              config.initialEvacuationMap.cooked.map((ti, to) => toTransactionInput(ti) -> to).toMap
            )
        } yield new EutxoL2Ledger(config, ref)
    }
}

case class EutxoL2Ledger private (
    config: EutxoL2Ledger.Config,
    private val state: Ref[IO, EutxoL2Ledger.State]
) extends VirtualLedger[IO] {
    implicit def monadF: Monad[IO] = Async[IO]

    override def sendInternalRequest(
        payload: Array[Byte],
        time: QuantizedInstant
    ): IO[Either[VirtualLedgerError, (Vector[EvacuationDiff], Vector[Payout.Obligation])]] = {
        (for {
            s <- EitherT.right(state.get)
            l2Tx <- EitherT.fromEither(
              L2Tx.parse(payload)
                  .left
                  .map(error =>
                      VirtualLedgerError(
                        error.getBytes
                      )
                  )
            )

            newState <- EitherT.fromEither(
              HydrozoaTransactionMutator
                  .transit(
                    config = config,
                    time = time,
                    state = s,
                    l2Tx = l2Tx
                  )
                  .left
                  .map(error => VirtualLedgerError(error.toString.getBytes))
            )

            adds =
                newState
                    .removedAll(s.keys)
                    .map((ti, to) => EvacuationDiff.Update(ti.toEvacuationKey, KeepRaw(to)))
                    .toVector

            deletes =
                s.removedAll(newState.keys)
                    .map((ti, _) => EvacuationDiff.Delete(ti.toEvacuationKey))
                    .toVector

            _ <- EitherT.right(state.set(newState))
        } yield (
          adds ++ deletes,
          Vector.from(l2Tx.l1utxos.map((_, to) => Payout.Obligation(KeepRaw(to))))
        )).value
    }

    override def sendGenesisRequest(
        deposit: DepositTuple
    ): IO[Either[VirtualLedgerError, Vector[EvacuationDiff]]] = {
        val genesisEvent =
            L2Genesis.fromDepositTuple(deposit)
        val evacuationDiffs = genesisEvent.asUtxos
            .map((ti, krto) => EvacuationDiff.Update(ti.toEvacuationKey, krto))
        IO.pure(Right(Vector.from(evacuationDiffs)))
    }
}
