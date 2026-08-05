package hydrozoa.integration.e2e

/** [[DockerHeadSuite]] on a 4-head-peer, no-coil head — `keygen-fleet 4 0 0`, hence the name and
  * that of its compose file (HEADS-COILS-QUORUM, the argument order).
  *
  * '''Why a second Docker suite.''' [[DockerSmokeTest]] runs what we ship, and must keep doing so —
  * its value is that a failure implicates DEPLOYMENT.md. But the shipped head has two head peers,
  * and only head peers publish the HTTP API, so it can only ever assert propagation across two. A
  * bug that needs three or four peers to show — a fan-out that drops the last recipient, a quorum
  * off-by-one — is invisible to it. This suite trades away the "same as production" property to buy
  * four HTTP-observable peers, and says so in its name rather than quietly reshaping the smoke
  * test's topology.
  *
  * The quorum is 0 because there is nothing to reach one with: the head multisig is all head peers
  * plus `MOf(coilQuorum, coilPeerVKeys)`, and head peers sign unanimously — so with no coil peers,
  * 0 is the only value `HeadConfig` accepts. Coil fan-out is therefore *not* covered here; that is
  * the smoke test's job.
  *
  * Heavy and CI-excluded by FQN in `build.sbt`, like every [[DockerHeadSuite]]. Run it with:
  * {{{
  *   HYDROZOA_INCLUDE_HEAVY_TESTS=1 sbt Docker/publishLocal stage \
  *     "integration/testOnly hydrozoa.integration.e2e.Docker400PropagationTest"
  * }}}
  */
class Docker400PropagationTest extends DockerHeadSuite(Docker400PropagationTest.topology)

object Docker400PropagationTest:

    /** Four head peers on their own compose file, under a project name of their own so containers
      * and networks stay clear of a [[DockerSmokeTest]] run — or of an operator's own project.
      *
      * Host ports are shared, though: the suite reaches peer `i` at `localhost:${8080 + i}`, and
      * the devnet overlay always publishes 18080 and 10000. So the two suites cannot run at the
      * same time. Both are minutes-long and run one at a time anyway.
      */
    private val topology: DockerTopology = DockerTopology(
      name = "4 head + 0 coil",
      heads = 4,
      coils = 0,
      coilQuorum = 0,
      project = "hydrozoa-e2e-400",
      tag = "[e2e-400]",
      composeFileNames = List(
        "integration/src/test/resources/e2e/docker-compose.4-0-0.yml",
        "docker-compose.yaci.yml"
      )
    )

end Docker400PropagationTest
