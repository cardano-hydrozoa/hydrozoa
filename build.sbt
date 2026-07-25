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
        .Try(scala.sys.process.Process("git describe --always --dirty --abbrev=8").!!.trim)
        .getOrElse("unknown")

// Bake the clean-output JVM flags into the generated launcher scripts (both `stage` and the Docker
// image), matching the `run` / `Test` settings: silence blst-java's JNI `System::load` and Scala
// `LazyVals`' `sun.misc.Unsafe` restricted-method warnings.
bashScriptExtraDefines ++= Seq(
  """addJava "--enable-native-access=ALL-UNNAMED"""",
  """addJava "--sun-misc-unsafe-memory-access=allow""""
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

val scalusVersion = "0.18.1"
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
                .Try(scala.sys.process.Process("git describe --always --dirty --abbrev=8").!!.trim)
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
      // Yaci suite requires a running Yaci DevKit instance; exclude from default test run.
      // Run explicitly with: integration/testOnly hydrozoa.integration.stage1.Stage1PropertiesYaci
      // NB: using * with testOnly still respects the excluded tests
      Test / testOptions += Tests.Exclude(
        Seq("hydrozoa.integration.stage1.Stage1PropertiesYaci")
      ),
      // test dependencies
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
// TODO: Restore integration module to fmt and lint
//addCommandAlias("fmtAll", ";core/scalafmtAll ;integration/scalafmtAll ;benchmark/scalafmtAll")
//addCommandAlias("fmtCheckAll", ";core/scalafmtCheckAll ;integration/scalafmtCheckAll ;benchmark/scalafmtCheckAll")
//addCommandAlias("lintAll", ";core/scalafixAll ;integration/scalafixAll ;benchmark/scalafixAll")
//addCommandAlias("lintCheckAll", ";core/scalafixAll --check ;integration/scalafixAll --check ;benchmark/scalafixAll --check")
addCommandAlias("fmtAll", ";core/scalafmtAll")
addCommandAlias("fmtCheckAll", ";core/scalafmtCheckAll")
addCommandAlias("lintAll", ";core/scalafixAll")
addCommandAlias("lintCheckAll", ";core/scalafixAll --check")

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
    version := "0.1.0",
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
