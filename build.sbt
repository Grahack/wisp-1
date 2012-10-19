import AssemblyKeys._

name := "Wisp"

version := "0.1-SNAPSHOT"

scalaVersion := "2.9.2"

resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies ++= Seq("org.specs2" %% "specs2" % "1.12.2" % "test")

scalacOptions ++= Seq("-deprecation")

jarName in assembly := "wisp.jar"

assemblySettings
