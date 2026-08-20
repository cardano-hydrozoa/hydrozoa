package hydrozoa.integration.e2e

import cats.effect.IO
import java.nio.file.Path
import org.http4s.client.Client

/** The Docker smoke-test: [[DockerHeadSuite]] run against the topology we actually ship — 2 head
  * peers and 4 coil peers, `keygen-fleet 2 4 2`, the scaffolded `docker-compose.yml` plus the
  * devnet overlay. A failure here is a failure of the procedure in docs/user-guide/DEPLOYMENT.md,
  * not of a test-only lookalike.
  *
  * The scenario is the baseline one: a single L2 transaction submitted to head-0 must reach every
  * head peer's L2 ledger. [[DockerRecoveryTest]] runs the same deployment through a crash.
  *
  * Heavy and CI-excluded; run it with `just integration-e2e-docker`. See [[DockerHeadSuite]] for
  * what the bring-up does and `docs/spec/integration-stages.md` for where this sits among the test
  * levels.
  */
class DockerSmokeTest
    extends DockerHeadSuite(DockerTopology.shipped, "an L2 tx reaches every head peer"):

    override protected def scenario(home: Path, client: Client[IO]): IO[Unit] =
        for {
            wallets <- loadL2Wallets(home)
            sent <- sendAda(wallets, client)
            _ <- awaitPropagation(client, sent)
            _ <- log("propagation confirmed on every head peer ✓")
        } yield ()

end DockerSmokeTest
