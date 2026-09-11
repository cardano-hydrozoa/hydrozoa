addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.6.1")
addSbtPlugin("com.github.sbt" % "sbt-native-packager" % "1.11.7")
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.13.1")
addSbtPlugin("ch.epfl.scala" % "sbt-scalafix" % "0.14.7")
addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.4.8")

// ScalaPB — generates the Request journal's record codec from `proto/request_record.proto`.
// The sbt 2 support lives in sbt-protoc 1.1.x and ScalaPB 1.0.0, both pre-release; the 0.11.x
// line's compilerplugin pulls `protoc-bridge_2.13` and cannot resolve alongside an sbt 2 plugin.
addSbtPlugin("com.thesamet" % "sbt-protoc" % "1.1.0-RC2")
libraryDependencies += "com.thesamet.scalapb" %% "compilerplugin" % "1.0.0-alpha.6"
