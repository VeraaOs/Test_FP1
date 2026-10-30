ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.8"

lazy val root = (project in file("."))
  .settings(
    name := "report-shell",
    libraryDependencies += "com.lihaoyi" %% "upickle" % "3.1.3",
    fork := true,
    run / connectInput := true,
    javaOptions += "-Dfile.encoding=UTF-8"
  )