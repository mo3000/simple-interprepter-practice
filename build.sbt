ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.1.1"

lazy val root = (project in file("."))
  .settings(
    name := "pascal-interpreter",
    idePackagePrefix := Some("org.ball.mini")
  )

libraryDependencies += "org.scalatest" % "scalatest_3" % "3.2.12-RC2"
