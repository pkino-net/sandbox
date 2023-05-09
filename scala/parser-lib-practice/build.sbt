ThisBuild / organization := "com.p_kino"
ThisBuild / organizationName := "p_kino"
ThisBuild / scalaVersion := "3.2.1"
ThisBuild / version := "0.1.0-SNAPSHOT"

lazy val root = (project in file("."))
  .settings(
    name := "parser-lib-practice",
    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.3.0"
  )
