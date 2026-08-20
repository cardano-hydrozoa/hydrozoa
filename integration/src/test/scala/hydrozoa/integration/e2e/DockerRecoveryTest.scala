package hydrozoa.integration.e2e

import cats.effect.IO
import java.nio.file.Path
import org.http4s.client.Client
import scala.concurrent.duration.*

/** The Docker crash-recovery test: [[DockerHeadSuite]] on the shipped topology, run through the
  * loss of a head peer. Once the head is formed and an L2 transaction has confirmed, the test
  * SIGKILLs head-1, keeps using the head while it is gone, then starts the same container again on
  * the same named volume and asserts the head comes back whole.
  *
  * '''Why head-1.''' The head multisig is *every* head peer plus a coil quorum, so losing one of
  * two head peers is the outage that actually stops consensus — a coil peer's loss is absorbed by
  * the 2-of-4 quorum. head-1 is also HTTP-observable, so its recovered L2 state can be read
  * directly rather than inferred.
  *
  * '''Why `kill` and not `stop`.''' `docker compose kill` sends SIGKILL: no SIGTERM, no shutdown
  * hook, no flush — the store the peer boots from is the one an abrupt crash left. The daemon
  * treats it as a manual stop, so the compose file's `restart: on-failure` does not resurrect the
  * container behind the test's back.
  *
  * What the run asserts, in order:
  *   1. head-1 really died — exit 137 (128 + SIGKILL) and no answer on `/health`;
  *   2. head-0 keeps serving through the outage, and still accepts a submission;
  *   3. head-1 reaches `/ready` again after a plain restart, off its own store;
  *   4. the pre-crash transaction is still in head-1's recovered L2 view — nothing was lost;
  *   5. the transaction submitted *during* the outage confirms on every head peer — the backlog
  *      drained rather than being dropped;
  *   6. a fresh transaction submitted after recovery confirms too — the head is live again, not
  *      wedged in a state that merely looks healthy.
  *
  * '''What it deliberately does not assert.''' That head-0's own L2 view stays empty during the
  * outage. A peer's ledger commits a block when the block is *produced*, and only the next block
  * waits on soft-confirmation (`docs/spec/fast-consensus.md`), so a surviving leader may
  * legitimately have applied one more block before stalling. Convergence across peers is the
  * property this suite can check without pinning that internal.
  *
  * This is the black-box counterpart to the crash-restart coverage
  * `docs/spec/persistence-and-crash-recovery.md` §9 lists as not yet built: it exercises a whole
  * node reboot end to end rather than a single actor's recovery contract.
  *
  * Heavy and CI-excluded; run it with `just integration-e2e-docker-recovery`.
  */
class DockerRecoveryTest
    extends DockerHeadSuite(
      DockerTopology.shippedRecovery,
      "a SIGKILLed head peer recovers and consensus resumes"
    ):

    import DockerRecoveryTest.*

    override protected def scenario(home: Path, client: Client[IO]): IO[Unit] =
        for {
            wallets <- loadL2Wallets(home)

            // A confirmed transaction first, so the peer we are about to kill has L2 state that a
            // botched recovery would visibly lose.
            beforeCrash <- sendAda(wallets, client)
            _ <- awaitPropagation(client, beforeCrash)
            _ <- log(s"baseline tx ${beforeCrash.txIdHex} confirmed on every head peer ✓")

            _ <- log(s"SIGKILLing $Victim…")
            _ <- killPeer(home, Victim)
            exit <- peerExitCode(home, Victim)
            _ <- ensure(
              exit == SigkillExit,
              s"$Victim exited $exit, not $SigkillExit — it was not killed by SIGKILL"
            )
            _ <- pollUntil(s"$Victim to stop answering", DownTimeout, 2.seconds)(
              peerResponds(client, VictimIndex).map(!_)
            )
            _ <- log(s"$Victim is down (exit $exit) ✓")

            // The head runs on one head peer for a while: long enough that the outage is a real
            // one, with a submitted request waiting on the peer that is gone.
            duringOutage <- sendAda(wallets, client)
            _ <- log(s"submitted ${duringOutage.txIdHex} during the outage; dwelling $OutageDwell…")
            _ <- IO.sleep(OutageDwell)
            survivorUp <- peerResponds(client, SurvivorIndex)
            _ <- ensure(survivorUp, "head-0 stopped serving while head-1 was down")
            victimUp <- peerResponds(client, VictimIndex)
            _ <- ensure(!victimUp, s"$Victim answered while it was supposed to be down")

            // The restart: same container, same volume, so the peer boots off the store its crash
            // left behind.
            _ <- log(s"restarting $Victim…")
            _ <- startPeer(home, Victim)
            _ <- pollUntil("every head peer to be ready again", RecoverTimeout, 5.seconds)(
              allReady(client)
            )
            _ <- log(s"$Victim is ready again ✓")

            _ <- awaitPropagation(client, beforeCrash)
            _ <- log("the pre-crash tx is still in the recovered L2 view ✓")

            _ <- awaitPropagation(client, duringOutage)
            _ <- log("the tx submitted during the outage confirmed on every head peer ✓")

            afterRecovery <- sendAda(wallets, client)
            _ <- awaitPropagation(client, afterRecovery)
            _ <- log("a fresh tx confirmed after recovery — the head is live again ✓")
        } yield ()

end DockerRecoveryTest

object DockerRecoveryTest:

    /** The head peer the run kills. Losing one of the two head peers stops consensus, because the
      * head multisig needs every head peer.
      */
    private val Victim = "head-1"
    private val VictimIndex = 1

    /** The head peer that stays up, keeps serving the HTTP API, and takes the outage submission. */
    private val SurvivorIndex = 0

    /** A container killed by SIGKILL exits 128 + 9. */
    private val SigkillExit = 137

    /** How long the head runs with a head peer missing before the restart — long enough for the
      * outage submission to be genuinely stuck rather than merely in flight.
      */
    private val OutageDwell = 30.seconds

    private val DownTimeout = 1.minute

    /** Real-wall-clock budget for the restarted peer: it replays its journals and rejoins the mesh
      * before `/ready` flips back.
      */
    private val RecoverTimeout = 5.minutes

end DockerRecoveryTest
