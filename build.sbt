ThisBuild / organization := "com.github.nhweston"
ThisBuild / scalaVersion := "2.13.1"
ThisBuild / scalacOptions := Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-language:implicitConversions",
)

lazy val root =
  (project in file(".")).settings(
    name := "dafnydoc",
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "pprint" % "0.5.6",
      "com.typesafe.play" %% "play-json" % "2.8.1",
      "org.planet42" %% "laika-core" % "0.16.1",
      "org.scala-lang.modules" %% "scala-xml" % "1.2.0",
    ),
    assemblyJarName in assembly := "dafnydoc.jar",
    assemblyOutputPath in assembly := file(".") / "bin" / "dafnydoc.jar",
    mainClass in assembly := Some("com.github.nhweston.dfydoc.Main"),
    test in assembly := { },
    assemblyMergeStrategy in assembly := {
      case PathList("META-INF", "MANIFEST.MF") => MergeStrategy.discard
      case _ => MergeStrategy.first
    },
  )
