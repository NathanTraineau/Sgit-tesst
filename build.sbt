name := "Sgit"

version := "0.1"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test"
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.1" % "test"
scalaVersion := "2.13.1"
parallelExecution in Test := false


/*assemblyMergeStrategy in assembly := {
  case PathList("META-INF", xs @ _*) => MergeStrategy.discard
  case x => MergeStrategy.first
}*/
import sbtassembly.AssemblyPlugin.defaultUniversalScript
target in assembly := file("./jar")
assemblyJarName in assembly := "sgit.jar"
assemblyOption in assembly := (assemblyOption in assembly).value.copy(prependShellScript = Some(defaultUniversalScript(shebang = false)))