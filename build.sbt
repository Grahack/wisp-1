import AssemblyKeys._

name := "Wisp"

version := "0.1-SNAPSHOT"

scalaVersion := "2.9.2"

jarName in assembly := "wisp.jar"

scalacOptions ++= Seq("-deprecation", "-Ydependent-method-types", "-unchecked")

assemblySettings

