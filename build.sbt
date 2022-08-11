val scala3Version = "3.1.3"

Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val root = (project in file("."))
    .settings(
        name := "peano",
        organization := "privateblue",
        version := "0.1.0-SNAPSHOT",
        scalaVersion := scala3Version,
        libraryDependencies += "org.typelevel" %% "cats-core" % "2.7.0",
        libraryDependencies += "org.typelevel" %% "algebra" % "2.7.0",
        libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.10" % "test",
        libraryDependencies += "org.scalatest" %% "scalatest-flatspec" % "3.2.10" % "test"
    )
