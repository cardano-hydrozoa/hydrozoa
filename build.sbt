// Main application
lazy val core = (project in file("."))
    .settings(
      libraryDependencies ++= Seq(
        // Scalus
        "org.scalus" % "scalus_3" % scalusVersion,
        "org.scalus" % "scalus-bloxbean-cardano-client-lib_3" % scalusVersion,
        // Cardano Client library
        "com.bloxbean.cardano" % "cardano-client-lib" % "0.6.3",
        "com.bloxbean.cardano" % "cardano-client-backend-blockfrost" % "0.6.3",
        // Tapir for API definition
        "com.softwaremill.sttp.tapir" %% "tapir-netty-server-sync" % "1.11.14",
        "com.softwaremill.sttp.tapir" %% "tapir-swagger-ui-bundle" % "1.11.14",
        // Argument parsing
        "com.monovore" %% "decline" % "2.5.0",
        "org.slf4j" % "slf4j-simple" % "2.0.16",
        // Concurrency
        "com.softwaremill.ox" %% "core" % "0.5.11",
        // Logging
        "com.typesafe.scala-logging" %% "scala-logging" % "3.9.4",
        "ch.qos.logback" % "logback-classic" % "1.2.10",
        "org.scala-lang" %% "toolkit" % "0.7.0",
        // JSON
        "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core" % "2.33.2",
        "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % "2.33.2" % "compile-internal",
        "com.softwaremill.sttp.tapir" %% "tapir-jsoniter-scala" % "1.11.19"
      ),
      libraryDependencies ++= Seq(
        "org.scalameta" %% "munit" % "1.1.0" % Test,
        "org.scalameta" %% "munit-scalacheck" % "1.1.0" % Test,
        "org.scalacheck" %% "scalacheck" % "1.18.1" % Test,
        "org.scalactic" %% "scalactic" % "3.2.19",
        "org.scalatest" %% "scalatest" % "3.2.19" % Test
      )
    )

// Latest Scala 3 LTS version
ThisBuild / scalaVersion := "3.3.5"

ThisBuild / scalacOptions ++= Seq("-feature", "-deprecation", "-unchecked")

// Add the Scalus compiler plugin
addCompilerPlugin("org.scalus" %% "scalus-plugin" % scalusVersion)

// Test dependencies
ThisBuild / testFrameworks += new TestFramework("munit.Framework")
// Integration tests
lazy val integration = (project in file("integration"))
    .dependsOn(core) // your current subproject
    .settings(
      publish / skip := true,
      // test dependencies
      libraryDependencies ++= Seq(
        "org.scalameta" %% "munit" % "1.1.0" % Test
      )
    )
val scalusVersion = "0.8.5"
