package hydrozoa.multisig.ledger.virtual

import cats.*
import cats.data.*
import cats.syntax.all.*
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.multisig.ledger.dapp.utxo.DepositTuple
import hydrozoa.multisig.ledger.joint.obligation.Payout

private type EV[F[_], A] = EitherT[F, VirtualLedgerError, A]
private type SV[F[_], A] = cats.data.StateT[[X] =>> EV[F, X], EvacuationMap, A]

case class VirtualLedgerError(bytes: Array[Byte])

trait VirtualLedger[F[_]] {
    implicit def monadF: Monad[F]

//    def sendStartBlock(time : QuantizedInstant, blockNumber : BlockNumber) : F[Either[VirtualLedgerError, Unit]]

    // FIXME: This can probably just be EitherT directly
    // TODO: This should get split up into the 2-phase register + absorb/reject
    /** The implementation of this function must send the payload to the virtual ledger and update
      * the virtual ledger's state.
      * @return
      *   Either an error blob if the request could not be applied, or a vector of diffs to apply to
      *   the JointLedger's evacuation map.
      */
    def sendGenesisRequest(
        deposit: DepositTuple
    ): F[Either[VirtualLedgerError, Vector[EvacuationDiff]]]

    // FIXME: This can probably just be EitherT directly
    /** The implementation of this function must send the payload to the virtual ledger and update
      * the virtual ledger's state.
      *
      * @param payload
      *   An opaque blob to send to the L2 ledger
      * @return
      *   Either an error blob if the request could not be applied, or a vector of diffs to apply to
      *   the JointLedger's evacuation map.
      */
    def sendInternalRequest(
        payload: Array[Byte],
        time: QuantizedInstant
    ): F[Either[VirtualLedgerError, (Vector[EvacuationDiff], Vector[Payout.Obligation])]]

    case class LedgerM[A] private (private val unLedger: SV[F, A]) {

        private def map[B](f: A => B): LedgerM[B] = LedgerM(this.unLedger.map(f))

        private def flatMap[B](f: A => LedgerM[B]): LedgerM[B] =
            LedgerM(this.unLedger.flatMap(a => f(a).unLedger))

        def run(
            initialEvacuationMap: EvacuationMap
        ): F[Either[VirtualLedgerError, (EvacuationMap, A)]] =
            this.unLedger.run(initialEvacuationMap).value
    }

    object LedgerM {

        private val get: LedgerM[EvacuationMap] =
            LedgerM(cats.data.StateT.get)

        private def lift[A](e: EitherT[F, VirtualLedgerError, A]): LedgerM[A] =
            LedgerM(StateT.liftF(e))

        private def set(newEvacMap: EvacuationMap): LedgerM[Unit] =
            LedgerM(cats.data.StateT.set(newEvacMap))

        // TODO: Move this out of here. We want to calculate the diffs, collect them in the joint ledger, and apply
        //   them later
        /** Applies a genesis event, fully updating the Ledger's state */
        final def applyGenesisEvent(absorbedDeposit: DepositTuple): LedgerM[Unit] =
            for {
                s <- get
                evacDiffs <- lift(EitherT(sendGenesisRequest(absorbedDeposit)))
                newState = EvacuationMap.applyDiffs(s, evacDiffs)
                _ <- set(newState)
            } yield ()

        // TODO: Move this out of here. We want to calculate the diffs, collect them in the joint ledger, and apply
        //   them later
        final def applyInternalTx(
            payload: Array[Byte],
            time: QuantizedInstant
        ): LedgerM[Vector[Payout.Obligation]] =
            for {
                s <- get
                res <- lift(EitherT(sendInternalRequest(payload, time)))
                newState = EvacuationMap.applyDiffs(s, res._1)
                _ <- set(newState)
            } yield res._2
    }
}
