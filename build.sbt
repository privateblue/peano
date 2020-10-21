Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val root = (project in file("."))
    .settings(
        name := "peano",
        organization := "privateblue",
        libraryDependencies += "org.typelevel" % "cats-core_2.13" % "2.2.0",
        libraryDependencies += "org.typelevel" % "algebra_2.13" % "2.0.0",
        libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.2" % Test,
        scalaVersion := "0.27.0-RC1"
    )

scalacOptions ++= Seq(
    // I wish there was a way to delegate from Eql to cats.Order, but I couldn't find any
    // "-language:strictEquality"
)
