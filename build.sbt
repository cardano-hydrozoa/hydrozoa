val scalusVersion = "0.15.1"
val bloxbeanVersion = "0.7.1"
val http4sVersion = "0.23.32"

// Main application
lazy val core: Project = (project in file("."))
    .settings(
      resolvers +=
          "Sonatype OSS New Snapshots" at "https://central.sonatype.com/repository/maven-snapshots/",
      resolvers += Resolver.defaultLocal,
      resolvers += "jitpack" at "https://jitpack.io",
      libraryDependencies ++= Seq(
        // Scalus
        // Using `org.scalus" %% "scalus` gives an error when using locally vendored version.
        "org.scalus" % "scalus_3" % scalusVersion withSources (),
        "org.scalus" % "scalus-cardano-ledger_3" % scalusVersion withSources (),
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
        "com.github.suprnation.cats-actors" %% "cats-actors" % "2.0.1",
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
        // scodec for hex encoding
        "org.scodec" %% "scodec-bits" % "1.2.1",
        // "io.netty" % "netty-all" % "4.2.4.Final"
      ),
      libraryDependencies ++= Seq(
        "org.typelevel" %% "spire-laws" % "0.18.0" % Test,
        "org.typelevel" %% "discipline-scalatest" % "2.3.0" % Test,
        "org.scalatest" %% "scalatest" % "3.2.19" % Test,
        "org.scalatestplus" %% "scalacheck-1-18" % "3.2.19.0" % Test,
        "org.typelevel" %% "cats-effect-testkit" % "3.6.3" % Test,
        "org.scalus" % "scalus-testkit_3" % scalusVersion % Test,
        "dev.optics" %% "monocle-core" % "3.3.0" % Test,
        "dev.optics" %% "monocle-macro" % "3.3.0" % Test,
        "io.github.cdimascio" % "dotenv-java" % "3.0.0" % Test
      )
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
ThisBuild / scalaVersion := "3.3.6"

ThisBuild / scalacOptions ++= Seq(
  "-feature",
  "-deprecation",
  "-unchecked",
  // "-Werror",
  "-language:implicitConversions",
  "-Wvalue-discard",
  "-Wunused:all",
  "-Wall",
  "-Yretain-trees", // Essential for incremental compilation
  "-Ypartial-unification" // needed for http4s, see https://http4s.org/v1/docs/service.html#running-your-service
)

// Add the Scalus compiler plugin
addCompilerPlugin("org.scalus" % "scalus-plugin_3" % scalusVersion)

// Custom commands to format and lint all subprojects
addCommandAlias("fmtAll", ";core/scalafmtAll ;integration/scalafmtAll ;benchmark/scalafmtAll")
addCommandAlias("fmtCheckAll", ";core/scalafmtCheckAll ;integration/scalafmtCheckAll ;benchmark/scalafmtCheckAll")
addCommandAlias("lintAll", ";core/scalafixAll ;integration/scalafixAll ;benchmark/scalafixAll")
addCommandAlias("lintCheckAll", ";core/scalafixAll --check ;integration/scalafixAll --check ;benchmark/scalafixAll --check")

// Test dependencies
ThisBuild / testFrameworks += new TestFramework("org.scalatest.tools.Framework")

inThisBuild(
  List(
    scalaVersion := "3.3.6",
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
        "org.scalus" % "scalus-testkit_3" % scalusVersion
      )
    )

// An attempt in exclude some folder
excludeFilter in Global := {
    val default = (excludeFilter in Global).value
    default || ".direnv" || ".bloop" || ".metals" || ".idea" || ".vscode"
}
