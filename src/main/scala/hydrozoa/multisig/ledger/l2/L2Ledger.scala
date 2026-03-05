package hydrozoa.multisig.ledger.l2

import cats.*
import cats.data.*
import cats.syntax.all.*
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.multisig.ledger.joint.obligation.Payout
import hydrozoa.multisig.ledger.joint.{EvacuationDiff, EvacuationMap}
import hydrozoa.multisig.ledger.l1.utxo.DepositTuple

private type EV[F[_], A] = EitherT[F, L2LedgerError, A]
private type SV[F[_], A] = cats.data.StateT[[X] =>> EV[F, X], EvacuationMap, A]

case class L2LedgerError(bytes: Array[Byte])

trait L2Ledger[F[_]] {
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
    ): F[Either[L2LedgerError, Vector[EvacuationDiff]]]

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
    ): F[Either[L2LedgerError, (Vector[EvacuationDiff], Vector[Payout.Obligation])]]

    case class L2LedgerM[A] private (private val unLedger: SV[F, A]) {

        private def map[B](f: A => B): L2LedgerM[B] = L2LedgerM(this.unLedger.map(f))

        private def flatMap[B](f: A => L2LedgerM[B]): L2LedgerM[B] =
            L2LedgerM(this.unLedger.flatMap(a => f(a).unLedger))

        def run(
            initialEvacuationMap: EvacuationMap
        ): F[Either[L2LedgerError, (EvacuationMap, A)]] =
            this.unLedger.run(initialEvacuationMap).value
    }

    object L2LedgerM {

        private val get: L2LedgerM[EvacuationMap] =
            L2LedgerM(cats.data.StateT.get)

        private def lift[A](e: EitherT[F, L2LedgerError, A]): L2LedgerM[A] =
            L2LedgerM(StateT.liftF(e))

        private def set(newEvacMap: EvacuationMap): L2LedgerM[Unit] =
            L2LedgerM(cats.data.StateT.set(newEvacMap))

        // TODO: Move this out of here. We want to calculate the diffs, collect them in the joint ledger, and apply
        //   them later
        /** Applies a genesis event, fully updating the Ledger's state */
        final def applyGenesisEvent(absorbedDeposit: DepositTuple): L2LedgerM[Unit] =
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
        ): L2LedgerM[Vector[Payout.Obligation]] =
            for {
                s <- get
                res <- lift(EitherT(sendInternalRequest(payload, time)))
                newState = EvacuationMap.applyDiffs(s, res._1)
                _ <- set(newState)
            } yield res._2
    }
}
