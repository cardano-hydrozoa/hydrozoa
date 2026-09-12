package hydrozoa.config.node

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import hydrozoa.config.ScriptReferenceUtxos
import hydrozoa.multisig.backend.cardano.CardanoBackend
import org.scalatest.funsuite.AnyFunSuite
import scala.concurrent.duration.DurationInt

/** The config load's retry loop.
  *
  * Loading a config resolves script reference UTxOs against Cardano, so a node that cannot reach
  * the backend exits — and a process supervisor answers by restarting it straight into the same
  * failure. The waits are a parameter here so the loop can be driven without waiting minutes.
  */
class NodeConfigRetryTest extends AnyFunSuite {

    private val backendDown: ScriptReferenceUtxos.Error =
        ScriptReferenceUtxos.Error.CardanoBackendError(
          CardanoBackend.Error.Unexpected("connection refused")
        )

    private val badConfig: io.circe.Error =
        io.circe.DecodingFailure("not a config", Nil)

    // Finite on purpose: production passes an INFINITE ladder (a transient backend failure must
    // never end in an exit), so the "gives up" cases below can only be expressed with a bounded one.
    private val waits = LazyList(1.milli, 1.milli, 1.milli)

    /** An attempt that fails `failures` times with `error`, then succeeds. */
    private def failing(
        failures: Int,
        error: ScriptReferenceUtxos.Error | io.circe.Error
    ): (IO[Either[ScriptReferenceUtxos.Error | io.circe.Error, String]], () => Int) =
        var attempts = 0
        val io = IO {
            attempts += 1
            if attempts <= failures then Left(error) else Right("loaded")
        }
        (io, () => attempts)

    test("a backend that comes back is retried until it does") {
        val (attempt, attempts) = failing(2, backendDown)
        val _ = assert(
          NodeConfig.retryingBackendReads(waits, attempt).unsafeRunSync() == Right("loaded")
        )
        assert(attempts() == 3)
    }

    test("a backend that stays down gives up with its own error, not a different one") {
        val (attempt, attempts) = failing(Int.MaxValue, backendDown)
        val _ = assert(
          NodeConfig.retryingBackendReads(waits, attempt).unsafeRunSync() == Left(backendDown)
        )
        // One attempt per wait, plus the first — not an unbounded loop.
        assert(attempts() == waits.length + 1)
    }

    test("a malformed config is not retried: re-reading the same bytes gives the same answer") {
        val (attempt, attempts) = failing(Int.MaxValue, badConfig)
        val _ = assert(
          NodeConfig.retryingBackendReads(waits, attempt).unsafeRunSync() == Left(badConfig)
        )
        assert(attempts() == 1)
    }

    test("a raised failure is retried too — building the backend reports failure by raising") {
        var attempts = 0
        val attempt: IO[Either[ScriptReferenceUtxos.Error | io.circe.Error, String]] = IO {
            attempts += 1
            if attempts <= 2 then
                throw new java.net.UnknownHostException("cardano-mainnet.blockfrost.io")
            else Right("loaded")
        }
        val _ = assert(
          NodeConfig.retryingBackendReads(waits, attempt).unsafeRunSync() == Right("loaded")
        )
        assert(attempts == 3)
    }

    test("a raise that never clears is re-raised, so the caller still sees the real cause") {
        val attempt: IO[Either[ScriptReferenceUtxos.Error | io.circe.Error, String]] =
            IO.raiseError(new java.net.UnknownHostException("cardano-mainnet.blockfrost.io"))
        val thrown = intercept[java.net.UnknownHostException](
          NodeConfig.retryingBackendReads(waits, attempt).unsafeRunSync()
        )
        assert(thrown.getMessage == "cardano-mainnet.blockfrost.io")
    }
}
