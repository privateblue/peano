ThisBuild / scalaVersion := "2.13.3"
ThisBuild / organization := "privateblue"

Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val root = (project in file("."))
  .settings(
    name := "peano",
    libraryDependencies += "org.typelevel" %% "cats-core" % "2.2.0",
    libraryDependencies += "org.typelevel" %% "algebra" % "2.0.0",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.0" % Test
  )
