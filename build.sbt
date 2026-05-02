name := "tip"

scalaVersion := "2.12.20"

trapExit := false

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Ywarn-unused-import", "-Ywarn-unused")

semanticdbEnabled := true
semanticdbVersion := scalafixSemanticdb.revision

libraryDependencies += "org.parboiled" %% "parboiled" % "2.1.8"
libraryDependencies += "com.regblanc" % "scala-smtlib_2.12" % "0.2.1"

Compile / scalaSource := baseDirectory.value / "src"
