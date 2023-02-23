ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.10"

lazy val root = (project in file("."))
  .settings(
    name := "PL_inclusion",
    idePackagePrefix := Some("ru.bmstu.iu9")
  )

libraryDependencies ++= Seq(
  "com.github.marianobarrios" %% "dregex" % "0.7.0",
  "org.slf4j" % "slf4j-api" % "2.0.5",
  "org.slf4j" % "slf4j-simple" % "2.0.5",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "2.2.0",
  "org.scala-lang.modules" %% "scala-collection-compat" % "2.8.1",
  "org.scala-lang" % "scala-reflect" % scalaVersion.value
)