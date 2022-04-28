
ThisBuild / scalaVersion     := "2.13.6"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "org.saswata"
ThisBuild / organizationName := "saswata"

lazy val root = (project in file("."))
  .settings(
    name := "parse-json"
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
