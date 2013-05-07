import AssemblyKeys._

name := "wisp"

version := "0.1-SNAPSHOT"

scalaVersion := "2.10.0"

libraryDependencies ++= Seq("org.specs2" %% "specs2" % "1.14" % "test",
        "junit" % "junit" % "4.7" % "test")

scalacOptions ++= Seq("-deprecation", "-feature")

assemblySettings
