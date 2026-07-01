package hydrozoa.multisig.backend.cardano

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import hydrozoa.lib.logging.ContraTracer
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
            result <- firewalled.submitTx(Transaction.empty)
            droppedEvents <- captured.get
        yield
            assert(
              result.isLeft,
              s"expected Left from underlying (empty tx rejected), got $result",
            )
            assert(droppedEvents.isEmpty)
        io.unsafeRunSync()
    }

    test("submitTx is short-circuited when shouldDrop = true; underlying is never asked") {
        val io = for
            underlying <- freshUnderlying
            captured <- Ref[IO].of(List.empty[FirewalledCardanoBackendEvent])
            sink = ContraTracer[IO, FirewalledCardanoBackendEvent](e => captured.update(e :: _))
            firewalled = new FirewalledCardanoBackend(underlying, _ => IO.pure(true), sink)
            result <- firewalled.submitTx(Transaction.empty)
            droppedEvents <- captured.get
        yield
            assert(result == Right(()), s"expected Right(()) short-circuit, got $result")
            assert(
              droppedEvents == List(
                FirewalledCardanoBackendEvent.DroppedOutboundTx(Transaction.empty.id)
              )
            )
        io.unsafeRunSync()
    }
