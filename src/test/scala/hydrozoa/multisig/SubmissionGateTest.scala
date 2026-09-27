package hydrozoa.multisig

import cats.effect.unsafe.implicits.global
import cats.effect.{Deferred, IO}
import org.scalatest.funsuite.AnyFunSuite
import scala.concurrent.duration.DurationInt

/** The gate the regime manager closes at the handoff to the rule-based regime, before it stops the
  * actors that answer user submissions.
  */
class SubmissionGateTest extends AnyFunSuite {

    private def run[A](io: IO[A]): A = io.timeout(10.seconds).unsafeRunSync()

    test("an open gate runs the submission and returns its answer") {
        assert(run(SubmissionGate().flatMap(_.admit("closed")(IO.pure("answered")))) == "answered")
    }

    test("a closed gate refuses without running the submission") {
        val (answer, ran) = run(for
            gate <- SubmissionGate()
            ran <- Deferred[IO, Unit]
            _ <- gate.closeAndDrain(1.second)
            answer <- gate.admit("closed")(ran.complete(()).as("answered"))
            ranAtAll <- ran.tryGet
        yield (answer, ranAtAll.isDefined))
        val _ = assert(answer == "closed")
        assert(!ran)
    }

    test("closing waits for a submission already admitted, which still gets its answer") {
        val (answer, drainedBeforeAnswer) = run(for
            gate <- SubmissionGate()
            release <- Deferred[IO, Unit]
            submission <- gate.admit("closed")(release.get.as("answered")).start
            _ <- IO.sleep(50.millis) // let it be admitted
            drainDone <- Deferred[IO, Unit]
            drain <- (gate.closeAndDrain(5.seconds) >> drainDone.complete(())).start
            _ <- IO.sleep(100.millis)
            drained <- drainDone.tryGet.map(_.isDefined)
            _ <- release.complete(())
            answer <- submission.joinWithNever
            _ <- drain.joinWithNever
        yield (answer, drained))
        val _ = assert(answer == "answered")
        assert(!drainedBeforeAnswer, "the drain must wait for the admitted submission")
    }

    test("a submission still waiting when the answering actors stop gets `closed`, not a hang") {
        val answer = run(for
            gate <- SubmissionGate()
            submission <- gate.admit("closed")(IO.never[String]).start
            _ <- IO.sleep(50.millis)
            _ <- gate.closeAndDrain(100.millis) // times out: the submission never finishes
            _ <- gate.markAnswerersStopped
            answer <- submission.joinWithNever
        yield answer)
        assert(answer == "closed")
    }
}
