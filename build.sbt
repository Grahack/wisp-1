import AssemblyKeys._

name := "Wisp"

version := "0.1-SNAPSHOT"

scalaVersion := "2.9.2"

libraryDependencies += "jline" % "jline" % "2.7"

scalacOptions ++= Seq("-deprecation", "-Ydependent-method-types", "-unchecked")

jarName in assembly := "wisp.jar"

assemblySettings

