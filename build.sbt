import AssemblyKeys._

name := "Wisp"

version := "0.1-SNAPSHOT"

scalaVersion := "2.9.2"

resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies ++= Seq("com.github.mdr" %% "ascii-graphs" % "0.0.1",
  "org.specs2" %% "specs2" % "1.12.2-SNAPSHOT" % "test",
  "org.pegdown" % "pegdown" % "1.0.2" % "test")

scalacOptions ++= Seq("-deprecation", "-Ydependent-method-types", "-unchecked")

jarName in assembly := "wisp.jar"

assemblySettings
