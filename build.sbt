name := "Sgit"

version := "0.1"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test"
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.1" % "test"
scalaVersion := "2.13.1"
parallelExecution in Test := false
libraryDependencies += "com.github.scopt" %% "scopt" % "4.0.0-RC2"


assemblyMergeStrategy in assembly := {
  case PathList("META-INF", xs @ _*) => MergeStrategy.discard
  case x => MergeStrategy.first
}
