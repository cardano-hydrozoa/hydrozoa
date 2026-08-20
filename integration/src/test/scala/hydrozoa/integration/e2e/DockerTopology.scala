package hydrozoa.integration.e2e

import java.nio.file.{Files, Path}

/** What distinguishes one [[DockerHeadSuite]] run from another: how many peers it generates, which
  * compose overlays layer onto the scaffolded deployment, and the project name isolating the
  * containers.
  *
  * The peer counts and the compose file are two statements of the same fact, and nothing reconciles
  * them — `keygen-fleet` writes configs for `heads + coils` peers, the compose file separately
  * declares services that mount them. Pair them here so a suite cannot silently disagree with the
  * file it starts.
  *
  * @param name
  *   how the topology reads in the test name, e.g. "shipped 2 head + 4 coil".
  * @param coilQuorum
  *   how many coil signatures a settlement needs. The head multisig is *all* head peers plus
  *   `MOf(coilQuorum, coilPeerVKeys)`, so this cannot exceed `coils` — `HeadConfig` refuses it —
  *   and must be 0 when there are none.
  * @param composeOverlayNames
  *   repo-relative compose overlays, layered onto the head directory's scaffolded
  *   `docker-compose.yml` in `docker compose -f` order.
  */
final case class DockerTopology(
    name: String,
    heads: Int,
    coils: Int,
    coilQuorum: Int,
    project: String,
    tag: String,
    composeOverlayNames: List[String]
) {
    require(
      coilQuorum <= coils,
      s"$name: coilQuorum $coilQuorum exceeds $coils coil peers — HeadConfig would refuse it"
    )

    /** The overlays, resolved against the repo root and checked to exist. */
    lazy val composeOverlays: List[Path] = composeOverlayNames.map { fileName =>
        val path = DockerHeadSuite.repoRoot.resolve(fileName)
        if !Files.exists(path) then throw RuntimeException(s"$path is missing")
        path
    }
}

object DockerTopology {

    /** The deployment `hydrozoa scaffold` writes and docs/user-guide/DEPLOYMENT.md walks through,
      * plus the devnet overlay: `keygen-fleet 2 4 2`.
      */
    val shipped: DockerTopology = DockerTopology(
      name = "shipped 2 head + 4 coil",
      heads = 2,
      coils = 4,
      coilQuorum = 2,
      project = "hydrozoa-e2e",
      tag = "[smoke]",
      composeOverlayNames = List("docker-compose.yaci.yml")
    )

    /** [[shipped]] again, under its own compose project. The project namespaces the peers' named
      * volumes, so a recovery run always starts from empty stores rather than resuming whatever a
      * smoke run left behind — which matters here, because the point of the run is what a peer
      * reads back from its own store.
      */
    val shippedRecovery: DockerTopology = shipped.copy(
      project = "hydrozoa-e2e-recovery",
      tag = "[recovery]"
    )
}
