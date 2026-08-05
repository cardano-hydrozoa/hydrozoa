enablePlugins(
  JavaAppPackaging,
  DockerPlugin,
  BuildInfoPlugin
)

Compile / mainClass := Some("hydrozoa.app.Main")
// Name the packaged launcher `hydrozoa`, and generate only the dispatcher's script (not forwarder
// scripts for every other discovered main); every command is a `hydrozoa <subcommand>`.
executableScriptName := "hydrozoa"
Compile / discoveredMainClasses := Seq("hydrozoa.app.Main")

// The git revision baked into the Docker image labels; matches `hydrozoa.BuildInfo.gitCommit`.
lazy val gitRevision: String =
    scala.util
        .Try(scala.sys.process.Process("git describe --tags --always --dirty --abbrev=8").!!.trim)
        .getOrElse("unknown")

// Bake the clean-output JVM flags into the generated launcher scripts (both `stage` and the Docker
// image), matching the `run` / `Test` settings: silence blst-java's JNI `System::load` and Scala
// `LazyVals`' `sun.misc.Unsafe` restricted-method warnings.
bashScriptExtraDefines ++= Seq(
  """addJava "--enable-native-access=ALL-UNNAMED"""",
  """addJava "--sun-misc-unsafe-memory-access=allow"""",
  // The one-shot CLI subcommands use a quiet, console-only logback config: no hydrozoa-trace.jsonl /
  // hydrozoa.log written into a possibly read-only cwd (e.g. inside the Docker image), and no
  // trace-level spam. Only `serve` keeps the verbose, file-writing logback.xml. `$1` here is the
  // subcommand (this runs before the launcher's own arg processing).
  """if [ "${1:-}" != "serve" ]; then addJava "-Dlogback.configurationFile=logback-cli.xml"; fi"""
)

// Docker settings
Docker / packageName := "cardano-hydrozoa/hydrozoa"
Docker / version := version.value
Docker / daemonUser := "hydrozoa"
Docker / daemonGroup := "hydrozoa"
// JDK 25 to match the project's language/runtime flags (--sun-misc-unsafe-memory-access is 23+).
dockerBaseImage := "eclipse-temurin:25-jre"
dockerExposedPorts ++= Seq(8080)

// Skip documentation generation for Docker
Compile / packageDoc / mappings := Seq()
Compile / doc / sources := Seq()

Global / excludeLintKeys += Docker / dockerLabels
Global / excludeLintKeys += Docker / dockerEnvVars

Docker / dockerLabels := Map(
  "org.opencontainers.image.title" -> "Hydrozoa",
  "org.opencontainers.image.description" -> "Cardano Hydrozoa L2 State Channel",
  "org.opencontainers.image.version" -> version.value,
  "org.opencontainers.image.revision" -> gitRevision
)

Docker / dockerEnvVars := Map(
  "JAVA_OPTS" -> "-Xmx2g -Xms512m"
)

// Ensure proper signal handling for graceful shutdown
import com.typesafe.sbt.packager.docker._
dockerCommands := dockerCommands.value.flatMap {
    case cmd @ Cmd("FROM", _) => List(cmd, Cmd("STOPSIGNAL", "SIGTERM"))
    case other                => List(other)
}

val scalusVersion = "1.0.0"
val bloxbeanVersion = "0.7.1"
val http4sVersion = "0.23.32"
val tapirVersion = "1.13.25"

// Cardano on-chain validators and shared on-chain types
lazy val cardanoOnchain: Project = (project in file("cardano-onchain"))
    .settings(
      name := "hydrozoa-cardano-onchain",
      publish / skip := true,
      resolvers +=
          "Sonatype OSS New Snapshots" at "https://central.sonatype.com/repository/maven-snapshots/",
      resolvers += Resolver.defaultLocal,
      libraryDependencies ++= Seq(
        "org.scalus" %% "scalus" % scalusVersion withSources (),
        "org.scalus" %% "scalus-cardano-ledger" % scalusVersion withSources (),
        "org.typelevel" %% "cats-core" % "2.13.0",
      ),
      addCompilerPlugin("org.scalus" % "scalus-plugin" % scalusVersion cross CrossVersion.full),
    )

// Standalone petri net framework
lazy val petri: Project = (project in file("petri"))
    .settings(
      name := "hydrozoa-petri",
      organization := "org.cardano-hydrozoa",
      version := "0.1.0-SNAPSHOT",
      libraryDependencies ++= Seq(
        "org.typelevel" %% "cats-core" % "2.13.0",
        "org.typelevel" %% "spire" % "0.18.0",
        "org.scalatest" %% "scalatest" % "3.2.19" % Test,
        "org.scalatestplus" %% "scalacheck-1-18" % "3.2.19.0" % Test,
        "org.typelevel" %% "discipline-scalatest" % "2.3.0" % Test,
        "org.typelevel" %% "spire-laws" % "0.18.0" % Test
      )
    )

// Main application
lazy val core: Project = (project in file("."))
    .dependsOn(cardanoOnchain)
    .settings(
      resolvers +=
          "Sonatype OSS New Snapshots" at "https://central.sonatype.com/repository/maven-snapshots/",
      resolvers += Resolver.defaultLocal,
      resolvers += "jitpack" at "https://jitpack.io",
      libraryDependencies ++= Seq(
        // Scalus
        "org.scalus" %% "scalus" % scalusVersion withSources (),
        "org.scalus" %% "scalus-cardano-ledger" % scalusVersion withSources (),
        // Cardano Client library
        "com.bloxbean.cardano" % "cardano-client-backend-blockfrost" % bloxbeanVersion,
        // Logging
        "ch.qos.logback" % "logback-classic" % "1.5.18",
        "org.typelevel" %% "log4cats-slf4j" % "2.7.1",
        // Used for input/output
        "org.scala-lang" %% "toolkit" % "0.7.0",
        // cats
        "org.typelevel" %% "cats-core" % "2.13.0",
        "org.typelevel" %% "cats-effect" % "3.6.3",
        "com.github.suprnation.cats-actors" %% "cats-actors" % "2.1.0",
        "org.typelevel" %% "spire" % "0.18.0",
        "org.scalactic" %% "scalactic" % "3.2.19",
        "org.typelevel" %% "cats-core" % "2.13.0",
        // http4s - web server and websocket client
        "org.http4s" %% "http4s-ember-server" % http4sVersion,
        "org.http4s" %% "http4s-ember-client" % http4sVersion,
        "org.http4s" %% "http4s-jdk-http-client" % "0.9.1",
        "org.http4s" %% "http4s-dsl" % http4sVersion,
        "org.http4s" %% "http4s-circe" % http4sVersion,
        "com.comcast" %% "ip4s-core" % "3.6.0",
        // tapir - endpoints-as-values, self-documenting REST API + Swagger UI (CE3 / http4s 0.23)
        "com.softwaremill.sttp.tapir" %% "tapir-http4s-server" % tapirVersion,
        "com.softwaremill.sttp.tapir" %% "tapir-json-circe" % tapirVersion,
        "com.softwaremill.sttp.tapir" %% "tapir-swagger-ui-bundle" % tapirVersion,
        "com.softwaremill.sttp.tapir" %% "tapir-openapi-docs" % tapirVersion,
        "com.softwaremill.sttp.apispec" %% "openapi-circe-yaml" % "0.11.10",
        // circe for JSON
        "io.circe" %% "circe-core" % "0.14.10",
        "io.circe" %% "circe-generic" % "0.14.10",
        "io.circe" %% "circe-parser" % "0.14.10",
        // upickle, to wrap scalus's blockfrost encoding
        "com.lihaoyi" %% "upickle" % "4.4.3",
        // scodec for hex encoding
        "org.scodec" %% "scodec-bits" % "1.2.1",
        "io.github.cdimascio" % "dotenv-java" % "3.0.0",
        // decline-effect — CLI args for Main
        "com.monovore" %% "decline-effect" % "2.6.2",
        // RocksDB (persistence layer)
        "org.rocksdb" % "rocksdbjni" % "9.7.3",
      ),
      libraryDependencies ++= Seq(
        "org.typelevel" %% "spire-laws" % "0.18.0" % Test,
        "org.typelevel" %% "discipline-scalatest" % "2.3.0" % Test,
        "org.scalatest" %% "scalatest" % "3.2.19" % Test,
        "org.scalatestplus" %% "scalacheck-1-18" % "3.2.19.0" % Test,
        "org.typelevel" %% "cats-effect-testkit" % "3.6.3" % Test,
        "org.scalus" %% "scalus-testkit" % scalusVersion % Test,
        "dev.optics" %% "monocle-core" % "3.3.0" % Test,
        "dev.optics" %% "monocle-macro" % "3.3.0" % Test,
        "co.fs2" %% "fs2-io" % "3.12.2" % Test
      ),
      // Bake the version, git revision, and build time into `hydrozoa.BuildInfo` so they can be
      // logged at startup, served from `GET /version`, and stamped onto the Docker image labels.
      buildInfoPackage := "hydrozoa",
      buildInfoKeys := Seq[BuildInfoKey](
        version,
        BuildInfoKey.action("gitCommit") {
            scala.util
                .Try(
                    scala.sys.process
                        .Process("git describe --tags --always --dirty --abbrev=8")
                        .!!
                        .trim
                )
                .getOrElse("unknown")
        }
      ),
      // BuildTime forces a regenerate each build, keeping gitCommit current within a warm session.
      buildInfoOptions += BuildInfoOption.BuildTime,
      // Fork JVM to properly pass system properties
      run / fork := true,
      // Silence the JVM's restricted-method warnings (blst-java JNI `System::load`, Scala
      // `LazyVals` via `sun.misc.Unsafe`) for forked `run`/CLI invocations, matching Test.
      run / javaOptions ++= Seq(
        "--enable-native-access=ALL-UNNAMED",
        "--sun-misc-unsafe-memory-access=allow"
      ),
      // The interactive `submit-deposit` / `submit-l2-tx` subcommands read console prompts; wire
      // stdin through to the forked `sbt run` JVM. The packaged launcher gets stdin natively.
      run / connectInput := true,
      // Copy the repo-root `docker-compose.yml` + `hydrozoa.sh` into the `/scaffold/*` classpath
      // resources at build time (the template + script-refs defaults live directly under
      // src/main/resources/scaffold/). The `scaffold` subcommand materializes these for a
      // Docker-only user, and `build-head-config` reads the baked script-refs — no repo clone.
      Compile / resourceGenerators += Def.task {
          val out = (Compile / resourceManaged).value / "scaffold"
          IO.createDirectory(out)
          val copies = Seq(
            baseDirectory.value / "docker-compose.yml" -> out / "docker-compose.yml",
            baseDirectory.value / "hydrozoa.sh" -> out / "hydrozoa.sh"
          )
          copies.foreach { case (src, dst) => IO.copyFile(src, dst) }
          copies.map(_._2)
      }.taskValue,
      // Fork each test run into a fresh JVM: isolates native state (RocksDB JNI), the
      // cats-effect IORuntime, and daemon threads, and makes re-running a `testOnly` in a
      // warm sbt session actually re-run instead of reporting 0 tests.
      Test / fork := true,
      // Forking otherwise serializes the suites; keep sbt's parallel suite execution in the
      // single forked JVM so the full run isn't dramatically slower than in-process.
      Test / testForkedParallel := true,
      // Forked test JVMs don't inherit the sbt launcher's JDK-25 flags, so grant the same
      // native access (blst-java JNI `System::load`, Scala `LazyVals` via `sun.misc.Unsafe`)
      // to keep test output free of restricted-method warnings.
      Test / javaOptions ++= Seq(
        "--enable-native-access=ALL-UNNAMED",
        "--sun-misc-unsafe-memory-access=allow"
      ),
    )

// Integration tests
lazy val integration: Project = (project in file("integration"))
    .dependsOn(core % "compile->compile;test->test", petri)
    .settings(
      // Compile / mainClass := Some("hydrozoa.demo.Workload"),
      publish / skip := true,
      // Heavy suites needing external infra (a running Yaci DevKit; Docker + the built image),
      // excluded from the default test run. sbt's Tests.Exclude filters a suite out even when it is
      // named explicitly by `testOnly <FQN>`, so the exclusion is lifted when
      // HYDROZOA_INCLUDE_HEAVY_TESTS=1 is set — that is how the `just` recipes run one by FQN:
      //   HYDROZOA_INCLUDE_HEAVY_TESTS=1 sbt "integration/testOnly …Stage1PropertiesYaci"   (just integration-yaci)
      //   HYDROZOA_INCLUDE_HEAVY_TESTS=1 sbt "integration/testOnly …DockerSmokeTest"  (just integration-e2e-docker)
      Test / testOptions ++=
          (if (sys.env.get("HYDROZOA_INCLUDE_HEAVY_TESTS").contains("1")) Seq.empty
           else
               Seq(
                 Tests.Exclude(
                   Seq(
                     "hydrozoa.integration.stage1.Stage1PropertiesYaci",
                     "hydrozoa.integration.e2e.DockerSmokeTest"
                   )
                 )
               )),
      // test dependencies
      libraryDependencies ++= Seq(
        "org.scalatestplus" %% "scalacheck-1-18" % "3.2.19.0" % Test,
        "org.typelevel" %% "cats-effect" % "3.6.3" % Test
      )
    )

// Runnable examples + tutorials (Catalyst deliverable). Reuses the integration harness
// (MultiPeerHeadHarness, TestPeers, mock L1) in test scope; each example is a demo suite run with
// `sbtn "examples/testOnly *TransientTokenDemo*"`. See examples/README.md.
lazy val examples: Project = (project in file("examples"))
    .dependsOn(core % "compile->compile;test->test", integration % "test->test")
    .settings(
      name := "hydrozoa-examples",
      publish / skip := true,
      libraryDependencies ++= Seq(
        "org.scalatestplus" %% "scalacheck-1-18" % "3.2.19.0" % Test,
        "org.typelevel" %% "cats-effect" % "3.6.3" % Test
      )
    )

// Latest Scala 3 LTS version
ThisBuild / scalaVersion := "3.3.7"

ThisBuild / scalacOptions ++= Seq(
  "-feature",
  "-deprecation",
  "-unchecked",
  "-language:implicitConversions",
  "-Wvalue-discard",
  "-Wunused:all",
  "-Wall",
  // Scala 3.3.7 enables `-Wtostring-interpolation` (via `-Wall`), which flags many pre-existing
  // `s"...$x"` sites where `x` is not a String. Silenced (non-fatal under `-Werror`) pending a
  // dedicated cleanup pass; demote to a warning/error once those sites are fixed.
  "-Wconf:msg=interpolation uses toString:s",
  "-Yretain-trees", // Essential for incremental compilation
) ++ (if (sys.env.contains("CI")) Seq("-Werror") else Nil)

// Add the Scalus compiler plugin (published with CrossVersion.full → scalus-plugin_<full-scala-version>)
addCompilerPlugin("org.scalus" % "scalus-plugin" % scalusVersion cross CrossVersion.full)

// Custom commands to format and lint all subprojects
addCommandAlias(
  "fmtAll",
  ";cardanoOnchain/scalafmtAll ;petri/scalafmtAll ;core/scalafmtAll ;integration/scalafmtAll ;benchmark/scalafmtAll"
)
addCommandAlias(
  "fmtCheckAll",
  ";cardanoOnchain/scalafmtCheckAll ;petri/scalafmtCheckAll ;core/scalafmtCheckAll ;integration/scalafmtCheckAll ;benchmark/scalafmtCheckAll"
)
addCommandAlias(
  "lintAll",
  ";cardanoOnchain/scalafixAll ;petri/scalafixAll ;core/scalafixAll ;integration/scalafixAll ;benchmark/scalafixAll"
)
addCommandAlias(
  "lintCheckAll",
  ";cardanoOnchain/scalafixAll --check ;petri/scalafixAll --check ;core/scalafixAll --check ;integration/scalafixAll --check ;benchmark/scalafixAll --check"
)

// Test dependencies
ThisBuild / testFrameworks += new TestFramework("org.scalatest.tools.Framework")

// Tests are wall-clock/sleep-bound (real System timestamps in the cats-actors), so run more
// suites concurrently than CPU cores to overlap the waits. sbt's default caps concurrency at the
// core count (2 on GitHub runners), which leaves those waits un-overlapped; pin it to 6.
Global / concurrentRestrictions := Seq(
  Tags.limitAll(6),
  Tags.limit(Tags.ForkedTestGroup, 1)
)

inThisBuild(
  List(
    // Release version — drives the Docker image tag, `hydrozoa.BuildInfo.version`, and `GET
    // /version`. Bump here for a release (see RELEASE.md), then tag `v<version>`.
    version := "0.1.1",
    scalaVersion := "3.3.7",
    semanticdbEnabled := true,
    semanticdbVersion := scalafixSemanticdb.revision
  )
)

// Benchmark subproject
lazy val benchmark: Project = (project in file("benchmark"))
    .enablePlugins(JmhPlugin)
    .dependsOn(core) // access to your main code
    .settings(
      name := "hydrozoa-benchmark",
      publish / skip := true,
      fork := true,
      // optional: set JVM args for benchmarking
      javaOptions ++= Seq(
        "-Xms2G",
        "-Xmx16G",
        "-Xss16m",
        "-XX:+UseG1GC"
      ),
      libraryDependencies ++= Seq(
        // "org.scalacheck" %% "scalacheck" % "1.19.0",
        "org.scalatestplus" %% "scalacheck-1-18" % "3.2.19.0" % Test,
        "org.scalus" %% "scalus-testkit" % scalusVersion
      )
    )

// An attempt in exclude some folder
excludeFilter in Global := {
    val default = (excludeFilter in Global).value
    default || ".direnv" || ".bloop" || ".metals" || ".idea" || ".vscode"
}
