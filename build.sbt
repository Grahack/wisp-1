import AssemblyKeys._

name := "Wisp"

version := "0.1-SNAPSHOT"

scalaVersion := "2.10.0-M7"

resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies ++= Seq(//"com.github.mdr" %% "ascii-graphs" % "0.0.1",
  "org.specs2" %% "specs2" % "1.13-SNAPSHOT" % "test")

scalacOptions ++= Seq("-deprecation", "-unchecked")

jarName in assembly := "wisp.jar"

assemblySettings
