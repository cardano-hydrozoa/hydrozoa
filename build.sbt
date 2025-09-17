//enablePlugins(
//  JavaAppPackaging,
//  DockerPlugin
//)

val scalusVersion = "0.11.0+35-d2c991bc-SNAPSHOT"
val bloxbeanVersion = "0.7.0-beta4"

Compile / mainClass := Some("hydrozoa.HydrozoaNode")
// Docker / packageName := "cardano-hydrozoa/hydrozoa"
// dockerBaseImage := "openjdk:21-jdk"
// dockerExposedPorts ++= Seq(4937)
//dockerEnvVars ++= Map(("COCKROACH_HOST", "dev.localhost"))
//dockerExposedVolumes := Seq("/opt/docker/.logs", "/opt/docker/.keys")

// Main application
lazy val core = (project in file("."))
    .settings(
      resolvers +=
          "Sonatype OSS New Snapshots" at "https://central.sonatype.com/repository/maven-snapshots/",
      resolvers += "jitpack" at "https://jitpack.io",
      libraryDependencies ++= Seq(
        // Scalus
        "org.scalus" %% "scalus" % scalusVersion withSources (),
        "org.scalus" %% "scalus-cardano-ledger" % scalusVersion withSources (),
        "org.scalus" %% "scalus-bloxbean-cardano-client-lib" % scalusVersion withSources (),
        // Cardano Client library
        "com.bloxbean.cardano" % "cardano-client-lib" % bloxbeanVersion,
        "com.bloxbean.cardano" % "cardano-client-backend-blockfrost" % bloxbeanVersion,
        // Tapir for API definition
        // "com.softwaremill.sttp.tapir" %% "tapir-netty-server-sync" % "1.11.14",
        // "com.softwaremill.sttp.tapir" %% "tapir-swagger-ui-bundle" % "1.11.14",
        // STTP4 Ox
        // "com.softwaremill.sttp.client4" %% "ox" % "4.0.0-RC1", // FIXME: not the latest
        // Argument parsing
        // "com.monovore" %% "decline" % "2.5.0",
        // Concurrency
        // "com.softwaremill.ox" %% "core" % "0.5.11",
        // "com.softwaremill.ox" %% "mdc-logback" % "0.5.13",
        // Logging
        // "ch.qos.logback" % "logback-classic" % "1.4.14",
        // "com.typesafe.scala-logging" %% "scala-logging" % "3.9.4",
        // Used for input/output
        "org.scala-lang" %% "toolkit" % "0.7.0",
        // jsoniter + tapit-jsoniter
        // "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core" % "2.33.2",
        // "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % "2.33.2" % "compile-internal",
        // "com.softwaremill.sttp.tapir" %% "tapir-jsoniter-scala" % "1.11.19",
        // Prometheus Java "client"
        // "io.prometheus" % "prometheus-metrics-core" % "1.3.6",
        // "io.prometheus" % "prometheus-metrics-instrumentation-jvm" % "1.3.6",
        // "io.prometheus" % "prometheus-metrics-exporter-httpserver" % "1.3.6",
        // "io.bullet" %% "borer-core" % "1.12.0",
        // "io.bullet" %% "borer-derivation" % "1.12.0",
        // cats
        "org.typelevel" %% "cats-core" % "2.13.0",
        "org.typelevel" %% "cats-effect" % "3.5.7",
        "com.github.suprnation.cats-actors" %% "cats-actors" % "2.0.1"
        // "io.netty" % "netty-all" % "4.2.4.Final"
      ),
      libraryDependencies ++= Seq(
        "org.scalameta" %% "munit" % "1.1.0" % Test,
        "org.scalameta" %% "munit-scalacheck" % "1.1.0" % Test,
        "org.scalacheck" %% "scalacheck" % "1.18.1" % Test,
        "org.scalus" %% "scalus-testkit" % scalusVersion % Test,
        "dev.optics" %% "monocle-core" % "3.1.0" % Test,
        "dev.optics" %% "monocle-macro" % "3.1.0" % Test
      )
    )
// Integration tests
//lazy val integration = (project in file("integration"))
//    .dependsOn(core)
//    .settings(
//      Compile / mainClass := Some("hydrozoa.demo.Workload"),
//      publish / skip := true,
//      // test dependencies
//      libraryDependencies ++= Seq(
//        "com.softwaremill.sttp.tapir" %% "tapir-sttp-client4" % "1.11.25",
//        "org.scalameta" %% "munit" % "1.1.0" % Test,
//        "org.scalameta" %% "munit-scalacheck" % "1.1.0" % Test,
//        "org.scalacheck" %% "scalacheck" % "1.18.1" % Test
//      )
//    )

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
  "-Wall"
)

// Add the Scalus compiler plugin
addCompilerPlugin("org.scalus" %% "scalus-plugin" % scalusVersion)
// Demo workload
//lazy val demo = (project in file("demo"))
//    .dependsOn(core, integration)
//    .settings(
//      Compile / mainClass := Some("hydrozoa.demo.Workload"),
//      publish / skip := true,
//      libraryDependencies ++= Seq(
//        "org.scalacheck" %% "scalacheck" % "1.18.1"
//      )
//    )

// Test dependencies
ThisBuild / testFrameworks += new TestFramework("munit.Framework")

inThisBuild(
  List(
    scalaVersion := "3.3.6",
    semanticdbEnabled := true,
    semanticdbVersion := scalafixSemanticdb.revision
  )
)
