val dottyVersion = "0.27.0-RC1"
val scala213Version = "2.13.3"

Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val root = (project in file("."))
  .settings(
    name := "peano",
    organization := "privateblue",
    libraryDependencies += "org.typelevel" % "cats-core_2.13" % "2.2.0",
    libraryDependencies += "org.typelevel" % "algebra_2.13" % "2.0.0",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.2" % Test,
    scalaVersion := dottyVersion,
    crossScalaVersions := Seq(dottyVersion, scala213Version)
  )

scalacOptions ++= Seq(
  //"-source:3.0-migration"
)
