import com.typesafe.sbt.packager.docker._

// NB (sbt 2): bare settings written at the top level of build.sbt are applied to *every*
// subproject, not just the root. Anything specific to the root application (Docker packaging,
// mainClass, launcher script) therefore lives inside the `core` project's `.settings(...)` /
// `.enablePlugins(...)` block below, not at the top level.

// The git revision baked into the Docker image labels; matches `hydrozoa.BuildInfo.gitCommit`.
lazy val gitRevision: String =
    scala.util
        .Try(scala.sys.process.Process("git describe --tags --always --dirty --abbrev=8").!!.trim)
        .getOrElse("unknown")

Global / excludeLintKeys += Docker / dockerLabels
Global / excludeLintKeys += Docker / dockerEnvVars
// native-packager's JavaAppPackaging archetype propagates executableScriptName/name/sourceDirectory
// into the Debian/Rpm/Universal-docs/Universal-src packaging scopes. We only build the Universal
// `stage` output and the Docker image, so those scoped copies are never consumed; silence sbt's
// lintUnused for them rather than leaving noise on every load (the plugins can't be disabled — the
// native-packager graph requires them).
Global / excludeLintKeys ++= Set(
  Debian / executableScriptName,
  Debian / sourceDirectory,
  Rpm / daemonStdoutLogFile,
  Rpm / executableScriptName,
  Rpm / name,
  Rpm / sourceDirectory,
  Universal / executableScriptName,
  UniversalDocs / name,
  UniversalSrc / name,
  rpmScriptsDirectory
)

val scalusVersion = "1.0.0"
val bloxbeanVersion = "0.7.1"
val http4sVersion = "0.23.32"
val tapirVersion = "1.13.25"

// Cardano on-chain validators and shared on-chain types
// sbt 2 renamed the class ScalaCheck sniffs to recognise a forked run, so ScalaCheck falls back to
// a path whose per-property events sbt's forked worker collapses: a falsified property is reported
// as `Passed: Total 1` with exit code 0. `ScalaCheckFrameworkFixed` delegates to ScalaCheck and
// only consolidates event delivery — properties still run concurrently. It lives in the root-level
// `test` package of core's test sources, which `integration` and `examples` get via their
// `test->test` dependency. NB: not `hydrozoa.test` — that shadows the top-level `test` package for
// every file under `hydrozoa.*`, breaking `import test.Generators...` on a clean compile.
//
// A bare (non-`ThisBuild`) `testFrameworks` setting in this file reaches every project, so one
// registration covers them all; `checkScalaCheckFramework` asserts that it actually did. Remove
// both, and `ScalaCheckFrameworkFixed`, once the upstream fixes are in a resolved version.
lazy val scalaCheckFramework: TestFramework =
  new TestFramework("test.ScalaCheckFrameworkFixed")

lazy val useFixedScalaCheck: Setting[Seq[TestFramework]] =
  testFrameworks := testFrameworks.value.filterNot(_ == TestFrameworks.ScalaCheck) :+
    scalaCheckFramework

// Registering the wrapper is per-project, and forgetting it in a new subproject would silently
// restore the dropped-failure behaviour — a green build, the same signature as the bug itself. This
// check fails the run instead. It inspects every project, so it also covers projects that never
// applied `useFixedScalaCheck`, which is precisely the case a per-project check would miss.
lazy val checkScalaCheckFramework =
  taskKey[Unit]("Fail if any project registers ScalaCheck's own sbt framework.")

lazy val everyProject = ScopeFilter(inAnyProject)

useFixedScalaCheck

checkScalaCheckFramework := Def.uncached {
  val ids = thisProject.all(everyProject).value.map(_.id)
  val frameworks = (Test / testFrameworks).all(everyProject).value
  val classpaths = (Test / dependencyClasspath).all(everyProject).value
  val offenders = ids.zip(frameworks).zip(classpaths).collect {
    case ((id, fws), cp)
        if fws.contains(TestFrameworks.ScalaCheck) &&
          cp.exists(_.data.id.contains("scalacheck_")) =>
      id
  }
  if (offenders.nonEmpty) {
    sys.error(
      s"${offenders.mkString(", ")} registers ScalaCheck's own sbt framework, which drops all but " +
        "one property result per suite on a forked run — a falsified property is then reported as " +
        "passed with exit code 0. Add `useFixedScalaCheck` to those projects. Remove this check, " +
        "`useFixedScalaCheck` and `ScalaCheckFrameworkFixed` once build.sbt resolves an sbt or " +
        "ScalaCheck version carrying the upstream fixes: typelevel/scalacheck#1195 " +
        "(fork detection) or sbt/sbt#9642 (events keyed per task overwrite one another)."
    )
  }
}


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
      // The Scalus compiler plugin compiles on-chain `@Compile` code to UPLC. Scope it per-project
      // (here + `core`) rather than as a bare top-level setting: under sbt 2 a bare setting applies
      // to *every* subproject, and enabling the plugin on a project that lacks the `scalus` library
      // on its classpath (e.g. `petri`) fails compilation with `package scalus.compiler ... does not
      // have a member method compile`. Published with CrossVersion.full → scalus-plugin_<full-scala>.
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
    .enablePlugins(JavaAppPackaging, DockerPlugin, BuildInfoPlugin)
    .dependsOn(cardanoOnchain)
    .settings(
      // Generate the Request journal's record from `proto/request_record.proto` into src_managed.
      // sbt-protoc's default protoc (3.21.7) is dynamically linked and will not start in a lean
      // container; 3.25.8 ships self-contained. Everything here resolves from Maven like any other
      // dependency — nothing is added to the flake.
      PB.protocVersion := "3.25.8",
      Compile / PB.protoSources := Seq(baseDirectory.value / "proto"),
      Compile / PB.targets := Seq(scalapb.gen() -> (Compile / sourceManaged).value / "scalapb"),
      Compile / mainClass := Some("hydrozoa.app.Main"),
      // Name the packaged launcher `hydrozoa`, and generate only the dispatcher's script (not
      // forwarder scripts for every other discovered main); every command is a `hydrozoa <sub>`.
      executableScriptName := "hydrozoa",
      Compile / discoveredMainClasses := Seq("hydrozoa.app.Main"),
      // Bake the clean-output JVM flags into the generated launcher scripts (both `stage` and the
      // Docker image), matching the `run` / `Test` settings: silence blst-java's JNI `System::load`
      // and Scala `LazyVals`' `sun.misc.Unsafe` restricted-method warnings.
      bashScriptExtraDefines ++= Seq(
        """addJava "--enable-native-access=ALL-UNNAMED"""",
        """addJava "--sun-misc-unsafe-memory-access=allow"""",
        // Select the logback config. Local runs default to the verbose, file-writing logback.xml;
        // the Docker image sets HYDROZOA_LOGBACK=logback-docker.xml (see Docker/dockerEnvVars) for a
        // quiet, console-only config so no subcommand writes hydrozoa.log / hydrozoa-trace.jsonl into
        // the container's filesystem.
        """addJava "-Dlogback.configurationFile=${HYDROZOA_LOGBACK:-logback.xml}""""
      ),
      // sbt 2 stages under target/out/jvm/scala-<v>/core/; pin the local `stage` output to the
      // stable repo-root path the justfile's deployment recipes expect
      // (target/universal/stage/bin/hydrozoa). Docker packaging keeps its own staging dir. The key
      // is qualified because JavaAppPackaging and DockerPlugin both re-export `stagingDirectory`.
      Universal / com.typesafe.sbt.packager.Keys.stagingDirectory :=
          baseDirectory.value / "target" / "universal" / "stage",
      // Same for the Docker build context: sbt 2 would stage it under target/out/.../core/docker/,
      // but the release workflow's docker build-push-action expects `target/docker/stage`. Pin it.
      Docker / com.typesafe.sbt.packager.Keys.stagingDirectory :=
          baseDirectory.value / "target" / "docker" / "stage",
      // Docker settings
      Docker / packageName := "cardano-hydrozoa/hydrozoa",
      Docker / version := version.value,
      // Also tag local `just docker-image` builds under ghcr.io/… — at both :<version> and :latest —
      // so docker-compose's default image (ghcr.io/cardano-hydrozoa/hydrozoa:latest) resolves to the
      // local build without a HYDROZOA_IMAGE override. Only affects publishLocal — the release
      // workflow tags/pushes via docker/metadata-action off `Docker/stage`, so this leaves the
      // published tags untouched.
      dockerAliases ++= Seq(
        dockerAlias.value.withRegistryHost(Some("ghcr.io")),
        dockerAlias.value.withRegistryHost(Some("ghcr.io")).withTag(Some("latest"))
      ),
      Docker / daemonUser := "hydrozoa",
      Docker / daemonGroup := "hydrozoa",
      // JDK 25 to match the project's language/runtime flags (--sun-misc-unsafe-memory-access is 23+).
      dockerBaseImage := "eclipse-temurin:25-jre",
      dockerExposedPorts ++= Seq(8080),
      // Skip documentation generation for Docker
      Compile / packageDoc / mappings := Seq(),
      Compile / doc / sources := Seq(),
      Docker / dockerLabels := Map(
        "org.opencontainers.image.title" -> "Hydrozoa",
        "org.opencontainers.image.description" -> "Cardano Hydrozoa L2 State Channel",
        "org.opencontainers.image.version" -> version.value,
        "org.opencontainers.image.revision" -> gitRevision
      ),
      Docker / dockerEnvVars := Map(
        "JAVA_OPTS" -> "-Xmx2g -Xms512m",
        // Every subcommand in the image uses a quiet, console-only logback config so the container
        // writes no hydrozoa.log / hydrozoa-trace.jsonl; the launcher reads this (see
        // bashScriptExtraDefines above). Kept out of JAVA_OPTS so `docker run -e JAVA_OPTS=…` can't
        // clobber the log config.
        "HYDROZOA_LOGBACK" -> "logback-docker.xml"
      ),
      // Ensure proper signal handling for graceful shutdown, and set the container env vars.
      // NB: native-packager emits multi-stage `FROM <img> AS <stage>` (several args), so match
      // `Cmd("FROM", _*)` rather than the single-arg `Cmd("FROM", _)`. The multi-stage output also
      // drops the `dockerEnvVars` ENV commands, so inject them explicitly into the final stage
      // (before ENTRYPOINT); values are quoted so ones with spaces (JAVA_OPTS) stay a single value.
      Docker / dockerCommands := {
          val base = (Docker / dockerCommands).value.flatMap {
              case cmd @ Cmd("FROM", _*) => List(cmd, Cmd("STOPSIGNAL", "SIGTERM"))
              case other                 => List(other)
          }
          // native-packager's multi-stage output doesn't render `dockerEnvVars`, so append the ENV
          // commands to the final stage ourselves (ENV applies to the runtime env regardless of
          // position). Values are quoted so ones with spaces (JAVA_OPTS) stay a single value.
          val envCommands =
              (Docker / dockerEnvVars).value.toList
                  .sortBy(_._1)
                  .map((k, v) => Cmd("ENV", s"""$k="$v""""))
          base ++ envCommands
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
        // ScalaPB runtime, for the code generated from `proto/`. Pulls protobuf-java itself.
        "com.thesamet.scalapb" %% "scalapb-runtime" % "1.0.0-alpha.6",
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
      // Scalus compiler plugin — compiles on-chain `@Compile` code to UPLC (see `cardanoOnchain`
      // for why this is scoped per-project rather than declared once at the top level).
      addCompilerPlugin("org.scalus" % "scalus-plugin" % scalusVersion cross CrossVersion.full),
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
      // Yaci suites require Docker / a running Yaci DevKit instance, and the Preview suite requires
      // a live public testnet + a funded master wallet; exclude both from the default test run.
      // Run explicitly, e.g.: integration/testOnly hydrozoa.integration.stage1.Stage1PropertiesYaci
      // NB: using * with testOnly still respects the excluded tests
      Test / testOptions += Tests.Exclude(
        Seq(
          "hydrozoa.integration.stage1.Stage1PropertiesYaci",
          "hydrozoa.integration.yaci.YaciDevnetSmokeTest",
          "hydrozoa.integration.yaci.YaciSetupProbe",
          "hydrozoa.integration.yaci.YaciMultiPeerProbe",
          "hydrozoa.integration.rbr.mbt.RbrMbtPropertiesYaci",
          "hydrozoa.integration.rbr.mbt.RbrMbtPropertiesPublic"
        )
      ),
      // ScalaCheck tuning for the default integration run: 10 cases per property (ScalaCheck's
      // own default is 100) and drop the "(extended)" properties, which are a naming convention
      // nothing else enforces. Together they are the difference between ~25 minutes and hours.
      //
      // Scoped to the ScalaCheck framework, and it has to be. Passing them on the command line as
      // `integration/testOnly * -- -s 10 -f …` hands them to EVERY framework on the module, and
      // ScalaTest reads `-s` as "run this named suite" and throws:
      //   IllegalArgumentException: Specifying a suite (-s <suite>) … is not supported when
      //   running ScalaTest from sbt
      // That was invisible only because every ScalaTest suite in this module happened to be in the
      // Tests.Exclude list above, so none was ever discovered. The moment a ScalaTest suite is
      // added and not excluded — the recovery suites — the recipe dies in five seconds.
      //
      // A full, unfiltered run is the same opt-out the Yaci recipes already use:
      //   sbt "; set integration/Test/testOptions := Seq() ; integration/testOnly *"
      // Scoped to `scalaCheckFramework`, not `TestFrameworks.ScalaCheck`: testOptions are matched
      // by framework, so naming a framework this build does not register would silently drop the
      // tuning — 100 cases instead of 10, and `(extended)` properties back in the fast run.
      Test / testOptions += Tests.Argument(
        scalaCheckFramework,
        "-s",
        "10",
        "-f",
        "^(?!.*\\(extended\\))"
      ),
      // test dependencies
      libraryDependencies ++= Seq(
        "org.scalatestplus" %% "scalacheck-1-18" % "3.2.19.0" % Test,
        "org.typelevel" %% "cats-effect" % "3.6.3" % Test,
        // Bloxbean's `yaci-cardano-test:0.1.0` (transitively via scalus-testkit) pins
        // testcontainers-java to 1.17.6 → docker-java 3.2.13 → Docker Engine API 1.32, which
        // modern rootless dockerd (>=API 1.40) refuses. Declare a newer testcontainers directly
        // so `YaciCardanoContainer.start` can talk to the daemon.
        "org.testcontainers" % "testcontainers" % "1.21.3" % Test
      ),
      // yaci-cardano-test also drags in `org.testcontainers:junit-jupiter:1.17.6`, which the single
      // `testcontainers` bump above doesn't cover — leaving a split 1.17.6/1.21.3 testcontainers
      // surface on the test classpath. Force the companion module to match so the whole group
      // resolves to one version.
      dependencyOverrides += "org.testcontainers" % "junit-jupiter" % "1.21.3",
      // testcontainers' `DockerClientProviderStrategy.getClientForConfig` unconditionally forces
      // the shaded docker-java `apiVersion` to `VERSION_1_32` whenever the config resolves to
      // `UNKNOWN_VERSION` (a compat fallback for pre-1.24 daemons). Modern rootless dockerd
      // rejects `/v1.32/*` because its `MinAPIVersion` is 1.40. Setting the `api.version` system
      // property makes the config non-UNKNOWN, so the strategy skips the fallback and negotiates
      // properly. Must be forked so the property reaches the strategy static-init.
      Test / fork := true,
      Test / javaOptions ++= Seq(
        "--enable-native-access=ALL-UNNAMED",
        "--sun-misc-unsafe-memory-access=allow",
        "-Dapi.version=1.44"
      )
    )

// Runnable examples + tutorials (Catalyst deliverable). Reuses the integration harness
// (MultiPeerHeadHarness, TestPeers, mock L1) in test scope; each example is a demo suite run with
// `sbt "examples/testOnly *TransientTokenDemo*"`. See examples/README.md.
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

// NB (sbt 2): a bare setting is added to every subproject. Written as `ThisBuild / scalacOptions
// ++=`, each per-subproject addition would append to the *shared* ThisBuild scope, so the flags
// accumulate once per subproject (4×) and `-Werror` then fails on "flag set repeatedly". Keep it
// bare (no `ThisBuild /`): each subproject gets exactly one copy.
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

// No `testFrameworks` entry for ScalaTest: sbt's default list already carries it, as
// `TestFramework(org.scalatest.tools.Framework, org.scalatest.tools.ScalaTestFramework)`. Adding it
// again registers a *second* framework that resolves to the same runner, and sbt then runs every
// suite once per entry — twice.

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
    version := "0.1.9",
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
