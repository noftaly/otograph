val scala3Version = "3.2.2"

lazy val root = project
  .in(file("."))
  .settings(
      name := "otograph",
      version := "0.1.0-SNAPSHOT",

      scalaVersion := scala3Version,
      libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test,
      libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "3.0.0",
      scalacOptions ++= Seq("-target:jvm-18", "-Xlint:-unused"),
      javaOptions += "-Dfile.encoding=UTF8"
  )

