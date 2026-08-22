package hydrozoa.integration.e2e

/** The Docker smoke-test: [[DockerHeadSuite]] run against the topology we actually ship — 2 head
  * peers and 4 coil peers, `keygen-fleet 2 4 2`, the scaffolded `docker-compose.yml` plus the
  * devnet overlay. A failure here is a failure of the procedure in docs/user-guide/DEPLOYMENT.md,
  * not of a test-only lookalike.
  *
  * Heavy and CI-excluded; run it with `just integration-e2e-docker`. See [[DockerHeadSuite]] for
  * what the run does and `docs/spec/integration-stages.md` for where it sits among the test levels.
  */
class DockerSmokeTest extends DockerHeadSuite(DockerTopology.shipped)
