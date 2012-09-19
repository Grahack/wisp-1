import AssemblyKeys._

name := "Wisp"

version := "0.1-SNAPSHOT"

scalaVersion := "2.9.2"

libraryDependencies ++= Seq("com.github.mdr" %% "ascii-graphs" % "0.0.1",
  "org.scalaz" % "scalaz-core_2.9.2" % "7.0.0-M3",
  "org.specs2" %% "specs2" % "1.12.1" % "test",
  "org.pegdown" % "pegdown" % "1.0.2" % "test")

scalacOptions ++= Seq("-deprecation", "-Ydependent-method-types", "-unchecked")

jarName in assembly := "wisp.jar"

assemblySettings
