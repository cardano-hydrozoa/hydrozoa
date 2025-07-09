enablePlugins(
  JavaAppPackaging,
  DockerPlugin
)

Compile / mainClass := Some("hydrozoa.HydrozoaNode")
Docker / packageName := "cardano-hydrozoa/hydrozoa"
dockerBaseImage := "openjdk:21-jdk"
dockerExposedPorts ++= Seq(4937)
//dockerEnvVars ++= Map(("COCKROACH_HOST", "dev.localhost"))
//dockerExposedVolumes := Seq("/opt/docker/.logs", "/opt/docker/.keys")

val scalusVersion = "0.10.1+228-294456c3-SNAPSHOT"

// Latest Scala 3 LTS version
ThisBuild / scalaVersion := "3.3.6"

ThisBuild / scalacOptions ++= Seq("-feature", "-deprecation", "-unchecked")

// Add the Scalus compiler plugin
addCompilerPlugin("org.scalus" %% "scalus-plugin" % scalusVersion)

// Main application
lazy val core = (project in file("."))
    .settings(
      resolvers +=
          "Sonatype OSS New Snapshots" at "https://central.sonatype.com/repository/maven-snapshots/",
      resolvers +=
          "Sonatype OSS01 Snapshots" at "https://s01.oss.sonatype.org/content/repositories/snapshots",
      resolvers +=
          "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
      libraryDependencies ++= Seq(
        // Scalus
        "org.scalus" %% "scalus" % scalusVersion,
        "org.scalus" %% "scalus-cardano-ledger" % scalusVersion,
        "org.scalus" %% "scalus-bloxbean-cardano-client-lib" % scalusVersion,
        // Cardano Client library
        "com.bloxbean.cardano" % "cardano-client-lib" % "0.7.0-beta3-SNAPSHOT",
        "com.bloxbean.cardano" % "cardano-client-backend-blockfrost" % "0.7.0-beta3-SNAPSHOT",
        // Tapir for API definition
        "com.softwaremill.sttp.tapir" %% "tapir-netty-server-sync" % "1.11.14",
        "com.softwaremill.sttp.tapir" %% "tapir-swagger-ui-bundle" % "1.11.14",
        // STTP4 Ox
        "com.softwaremill.sttp.client4" %% "ox" % "4.0.0-RC1", // FIXME: not the latest
        // Argument parsing
        "com.monovore" %% "decline" % "2.5.0",
        // Concurrency
        "com.softwaremill.ox" %% "core" % "0.5.11",
        "com.softwaremill.ox" %% "mdc-logback" % "0.5.13",
        // Logging
        "ch.qos.logback" % "logback-classic" % "1.4.14",
        "com.typesafe.scala-logging" %% "scala-logging" % "3.9.4",
        // Used for input/output
        "org.scala-lang" %% "toolkit" % "0.7.0",
        // jsoniter + tapit-jsoniter
        "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core" % "2.33.2",
        "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % "2.33.2" % "compile-internal",
        "com.softwaremill.sttp.tapir" %% "tapir-jsoniter-scala" % "1.11.19",
        // Prometheus Java "client"
        "io.prometheus" % "prometheus-metrics-core" % "1.3.6",
        "io.prometheus" % "prometheus-metrics-instrumentation-jvm" % "1.3.6",
        "io.prometheus" % "prometheus-metrics-exporter-httpserver" % "1.3.6"
      ),
      libraryDependencies ++= Seq(
        "org.scalameta" %% "munit" % "1.1.0" % Test,
        "org.scalameta" %% "munit-scalacheck" % "1.1.0" % Test,
        "org.scalacheck" %% "scalacheck" % "1.18.1" % Test,
        "org.scalus" %% "scalus-testkit" % scalusVersion % Test
      )
    )

// Test dependencies
ThisBuild / testFrameworks += new TestFramework("munit.Framework")

// Integration tests
lazy val integration = (project in file("integration"))
    .dependsOn(core)
    .settings(
      Compile / mainClass := Some("hydrozoa.demo.Workload"),
      publish / skip := true,
      // test dependencies
      libraryDependencies ++= Seq(
        "com.softwaremill.sttp.tapir" %% "tapir-sttp-client4" % "1.11.25",
        "org.scalameta" %% "munit" % "1.1.0" % Test,
        "org.scalameta" %% "munit-scalacheck" % "1.1.0" % Test,
        "org.scalacheck" %% "scalacheck" % "1.18.1" % Test
      )
    )

// Demo workload
lazy val demo = (project in file("demo"))
    .dependsOn(core, integration)
    .settings(
      Compile / mainClass := Some("hydrozoa.demo.Workload"),
      publish / skip := true,
      libraryDependencies ++= Seq(
        "org.scalacheck" %% "scalacheck" % "1.18.1"
      )
    )
