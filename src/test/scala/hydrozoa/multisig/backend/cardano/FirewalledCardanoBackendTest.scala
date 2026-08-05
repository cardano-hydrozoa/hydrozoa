package hydrozoa.multisig.backend.cardano

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.ledger.l1.tx.RawTx
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.Transaction

class FirewalledCardanoBackendTest extends AnyFunSuite:

    // A fresh mock rejects an empty tx (mutator returns Left(InvalidTx)); we use the
    // pass-through Left vs. dropped Right(()) as the observable signal for which path ran.
    private def freshUnderlying: IO[CardanoBackend[IO]] =
        CardanoBackendMock.mockIO(MockState(scalus.cardano.ledger.Utxos.empty))

    test("submitTx passes through when shouldDrop = false; underlying rejects empty tx") {
        val io = for
            underlying <- freshUnderlying
            captured <- Ref[IO].of(List.empty[FirewalledCardanoBackendEvent])
            sink = ContraTracer[IO, FirewalledCardanoBackendEvent](e => captured.update(e :: _))
            firewalled = new FirewalledCardanoBackend(underlying, _ => IO.pure(false), sink)
            result <- firewalled.submitTx(RawTx(Transaction.empty))
            events <- captured.get
        yield
            val _ = assert(
              result.isLeft,
              s"expected Left from underlying (empty tx rejected), got $result",
            )
            val _ = assert(
              !events.exists(_.isInstanceOf[FirewalledCardanoBackendEvent.DroppedOutboundTx]),
              s"expected no drop event, got $events",
            )
            assert(
              events.collect { case s: FirewalledCardanoBackendEvent.SubmittedTx => s } ==
                  List(FirewalledCardanoBackendEvent.SubmittedTx(Transaction.empty.id, result)),
              s"expected one pass-through SubmittedTx recording the underlying result, got $events",
            )
        io.unsafeRunSync()
    }

    test("submitTx is short-circuited when shouldDrop = true; underlying is never asked") {
        val io = for
            underlying <- freshUnderlying
            captured <- Ref[IO].of(List.empty[FirewalledCardanoBackendEvent])
            sink = ContraTracer[IO, FirewalledCardanoBackendEvent](e => captured.update(e :: _))
            firewalled = new FirewalledCardanoBackend(underlying, _ => IO.pure(true), sink)
            result <- firewalled.submitTx(RawTx(Transaction.empty))
            events <- captured.get
        yield
            val _ = assert(result == Right(()), s"expected Right(()) short-circuit, got $result")
            assert(
              events == List(
                FirewalledCardanoBackendEvent.DroppedOutboundTx(Transaction.empty.id)
              ),
              s"expected exactly one DroppedOutboundTx (no SubmittedTx on the drop path), got $events",
            )
        io.unsafeRunSync()
    }
