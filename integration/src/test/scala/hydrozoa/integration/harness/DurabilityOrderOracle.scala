package hydrozoa.integration.harness

import cats.effect.{IO, Ref}
import cats.syntax.all.*
import hydrozoa.multisig.consensus.peer.PeerId
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.stack.StackNumber
import hydrozoa.multisig.persistence.{ArrivalStamp, BackendStore, JournalKey, Markers, Persistence, StoreKey, WriteBatch}
import java.time.Instant

/** A [[Persistence]] decorator that checks the boot-time durability-ordering invariants after
  * **every** durable write, instead of waiting for a crash to land on the one write that breaks
  * them.
  *
  * `ReplayActor.validateInvariants` refuses to start a peer whose store has `confirmed > acked` on
  * either arm — the fast arm `softConfirmed ≤ fastBlockMark`, the slow arm
  * `hardConfirmed ≤ hardAckedStack`. That gate reads the store once, at boot. But the property it
  * encodes is a property of the *write ordering*, so it has to hold at every instant the store is
  * observable: any moment it does not is a moment at which a process death would brick that peer.
  *
  * Crash-injection tests can only sample those moments — [[CrashingPersistence]] picks one op out
  * of thousands, and a window that is a handful of instructions wide is unlikely to be the one
  * sampled. This decorator inverts that: it evaluates the invariant at every observable point, so a
  * violated ordering is caught on the write that violates it, deterministically, without having to
  * guess where the window is.
  *
  * Violations are recorded rather than raised, so one run reports every point at which the store
  * was unrecoverable rather than only the first.
  */
object DurabilityOrderOracle:

    /** One observed moment at which the store would have failed the boot gate. `afterOp` is the
      * 1-based index of the durable op after which it was observed, to locate it in a trace.
      */
    final case class Violation(
        afterOp: Int,
        peer: PeerId,
        arm: String,
        confirmed: String,
        acked: String,
    ):
        override def toString: String =
            s"[$peer] after durable op $afterOp: $arm confirmed=$confirmed > acked=$acked"

    /** Wrap `inner` so every durable op is followed by an invariant check against the backend.
      * `own` scopes the own-hard-ack lane the slow arm reads, exactly as `ReplayActor` does.
      */
    def wrap(
        inner: Persistence[IO],
        own: PeerId,
        opCounter: Ref[IO, Int],
        violations: Ref[IO, List[Violation]],
    ): Persistence[IO] =
        new Persistence[IO]:
            private def checked(op: IO[Unit]): IO[Unit] =
                op >> opCounter.updateAndGet(_ + 1).flatMap(check)

            /** Both arms are checked by reading the CONFIRMED mark first and the ACKED mark second,
              * sequentially. That ordering is what makes a reported violation sound.
              *
              * Every one of these marks is monotone — they are `max(key)` over an append-only
              * column family — and the check runs while other fibers of the same peer are still
              * writing. So the acked mark, read second, is never smaller than its value at the
              * instant the confirmed mark was read: if `confirmed > acked` still holds across that
              * gap, it held at a single instant too. A violation is therefore real.
              *
              * The converse is a false negative — a genuine violation can close before the second
              * read sees it — which is the safe direction, and one this test tolerates because it
              * takes the check at every durable write rather than at one sampled point.
              *
              * `Markers.derive` is deliberately not used here: it issues its reads with `parMapN`,
              * so it can pair a stale acked mark with a fresh confirmed one and manufacture a
              * violation that never existed.
              */
            private def check(opNum: Int): IO[Unit] =
                for
                    softConfirmed <- Markers.recoverSoftConfirmed(inner.backend)
                    fastBlockMark <- Markers.recoverFastBlockMark(inner.backend)
                    _ <- record(opNum, "fast(soft)", softConfirmed, fastBlockMark)(
                      Ordering[BlockNumber]
                    )
                    hardConfirmed <- Markers.recoverHardConfirmed(inner.backend)
                    hardAcked <- Markers.recoverHardAcked(inner.backend, own)
                    hardAckedStack <- hardAcked.traverse(n =>
                        inner.getOrFail(JournalKey.HardAck(own, n)).map(_.payload.stackNum)
                    )
                    _ <- record(opNum, "slow(hard)", hardConfirmed, hardAckedStack)(
                      Ordering[StackNumber]
                    )
                yield ()

            /** Record a violation when both marks exist and `confirmed` outruns `acked`. A missing
              * mark is not a violation: the boot gate treats it the same way (a cold arm passes).
              */
            private def record[A](
                opNum: Int,
                arm: String,
                confirmed: Option[A],
                acked: Option[A],
            )(ord: Ordering[A]): IO[Unit] =
                (confirmed, acked) match
                    case (Some(c), Some(a)) if ord.gt(c, a) =>
                        violations.update(
                          _ :+ Violation(opNum, own, arm, c.toString, a.toString)
                        )
                    case _ => IO.unit

            def put(key: StoreKey)(value: key.Value): IO[Unit] = checked(inner.put(key)(value))

            def delete(key: StoreKey): IO[Unit] = checked(inner.delete(key))

            def write(batch: WriteBatch): IO[Unit] = checked(inner.write(batch))

            def get(key: StoreKey): IO[Option[key.Value]] = inner.get(key)

            def getOrFail(key: StoreKey): IO[key.Value] = inner.getOrFail(key)

            def arrivalStamp: IO[ArrivalStamp] = inner.arrivalStamp

            def zeroTimes: IO[Map[Int, Long]] = inner.zeroTimes

            def wallClockOf(stamp: ArrivalStamp): IO[Instant] = inner.wallClockOf(stamp)

            def backend: BackendStore[IO] = inner.backend
