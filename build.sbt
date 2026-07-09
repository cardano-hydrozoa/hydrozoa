// NB (sbt 2): bare settings written at the top level of build.sbt are applied to *every*
// subproject, not just the root. Anything specific to the root application (Docker packaging,
// mainClass) therefore lives inside the `core` project's `.settings(...)` block below, not here.
import com.typesafe.sbt.packager.docker._

Global / excludeLintKeys += Docker / dockerLabels
Global / excludeLintKeys += Docker / dockerEnvVars

val scalusVersion = "0.18.1"
val bloxbeanVersion = "0.7.1"
val http4sVersion = "0.23.32"

// Cardano on-chain validators and shared on-chain types
lazy val cardanoOnchain: Project = (project in file("cardano-onchain"))
    .settings(
      name := "hydrozoa-cardano-onchain",
      publish / skip := true,
      resolvers +=
          "Sonatype OSS New Snapshots" at "https://central.sonatype.com/repository/maven-snapshots/",
      resolvers += Resolver.defaultLocal,
      libraryDependencies ++= Seq(
        ("org.scalus" %% "scalus" % scalusVersion).withSources(),
        ("org.scalus" %% "scalus-cardano-ledger" % scalusVersion).withSources(),
        "org.typelevel" %% "cats-core" % "2.13.0",
      ),
      // NB (sbt 2): the top-level `addCompilerPlugin` below is a bare setting and is applied to
      // every subproject (including this one), so no per-project addition is needed here.
    )

// Main application
lazy val core: Project = (project in file("."))
    .enablePlugins(JavaAppPackaging, DockerPlugin)
    .dependsOn(cardanoOnchain)
    .settings(
      Compile / mainClass := Some("hydrozoa.app.Main"),
      // Docker settings
      Docker / packageName := "cardano-hydrozoa/hydrozoa",
      Docker / version := version.value,
      Docker / daemonUser := "hydrozoa",
      Docker / daemonGroup := "hydrozoa",
      // Use Debian-based image for better compatibility
      dockerBaseImage := "eclipse-temurin:21-jre-jammy",
      dockerExposedPorts ++= Seq(8080),
      // Skip documentation generation for Docker
      Compile / packageDoc / mappings := Seq(),
      Compile / doc / sources := Seq(),
      Docker / dockerLabels := Map(
        "org.opencontainers.image.title" -> "Hydrozoa",
        "org.opencontainers.image.description" -> "Cardano Hydrozoa L2 State Channel",
        "org.opencontainers.image.version" -> version.value
      ),
      Docker / dockerEnvVars := Map(
        "JAVA_OPTS" -> "-Xmx2g -Xms512m"
      ),
      // Ensure proper signal handling for graceful shutdown.
      // NB: native-packager emits multi-stage `FROM <img> AS <stage>` (several args), so match
      // `Cmd("FROM", _*)` rather than the single-arg `Cmd("FROM", _)`.
      dockerCommands := dockerCommands.value.flatMap {
          case cmd @ Cmd("FROM", _*) => List(cmd, Cmd("STOPSIGNAL", "SIGTERM"))
          case other                 => List(other)
      },
      resolvers +=
          "Sonatype OSS New Snapshots" at "https://central.sonatype.com/repository/maven-snapshots/",
      resolvers += Resolver.defaultLocal,
      resolvers += "jitpack" at "https://jitpack.io",
      libraryDependencies ++= Seq(
        // Scalus
        ("org.scalus" %% "scalus" % scalusVersion).withSources(),
        ("org.scalus" %% "scalus-cardano-ledger" % scalusVersion).withSources(),
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
      // Fork JVM to properly pass system properties
      run / fork := true,
    )

// Integration tests
lazy val integration: Project = (project in file("integration"))
    .dependsOn(core % "compile->compile;test->test")
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

// NB (sbt 2): a bare setting is added to every subproject. Written as `ThisBuild / scalacOptions
// ++=`, each of those per-subproject additions would append to the *shared* ThisBuild scope, so the
// flags accumulate once per subproject (4×) and `-Werror` then fails on "flag set repeatedly".
// Keep it bare (no `ThisBuild /`): each subproject gets exactly one copy.
scalacOptions ++= Seq(
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
addCommandAlias("fmtCheckAll", ";core/scalafmtCheckAll ")
addCommandAlias("lintAll", ";core/scalafixAll ")
addCommandAlias("lintCheckAll", ";core/scalafixAll --check ;")

// Test dependencies. Bare (not `ThisBuild /`) for the same reason as scalacOptions above: a
// `ThisBuild / … +=` bare statement accumulates once per subproject on the shared scope.
testFrameworks += new TestFramework("org.scalatest.tools.Framework")

inThisBuild(
  List(
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
Global / excludeFilter := {
    val default = (Global / excludeFilter).value
    default || ".direnv" || ".bloop" || ".metals" || ".idea" || ".vscode"
}
