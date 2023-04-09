ThisBuild / version := "0.2.0"

ThisBuild / scalaVersion := "3.2.2"

lazy val root = (project in file("."))
  .settings(
    name := "calculator"
  )
