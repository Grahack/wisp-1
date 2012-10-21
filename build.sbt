import AssemblyKeys._

name := "Wisp"

version := "0.1-SNAPSHOT"

scalaVersion := "2.10.0-RC1"

libraryDependencies ++= Seq("org.specs2" % "specs2_2.10.0-RC1" % "1.12.2" % "test")

scalacOptions ++= Seq("-deprecation")

jarName in assembly := "wisp.jar"

assemblySettings
