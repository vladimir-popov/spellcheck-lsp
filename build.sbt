import sbtassembly.AssemblyPlugin.defaultUniversalScript

ThisBuild / organization := "ru.dokwork"
ThisBuild / scalaVersion := "3.1.0"

ThisBuild / semanticdbEnabled := true
ThisBuild / semanticdbVersion := scalafixSemanticdb.revision
ThisBuild / scalafixDependencies += "com.github.liancheng" %% "organize-imports" % "0.6.0"

lazy val compilerOptions = Seq(
  "-encoding",
  "utf-8",
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Xfatal-warnings",
  "-Ykind-projector"
)

lazy val dependencies = new {
  val versions = new {
    val lsp4j = "0.12.0"
    val languagetool = "5.6"
    val decline = "2.2.0"
    val scalatest = "3.2.10" 
  }

  val languagetool = ("org.languagetool" % "language-en" % versions.languagetool).exclude("com.intellij", "annotations")
  val lsp4j =   "org.eclipse.lsp4j" % "org.eclipse.lsp4j" % versions.lsp4j
  val decline = "com.monovore" %% "decline" % versions.decline
  val scalatest = "org.scalatest" %% "scalatest" % versions.scalatest % "test"
}


lazy val `spellcheck-lsp` = (project in file("."))
  .settings(
    scalacOptions ++= compilerOptions,
    libraryDependencies ++= Seq(
      dependencies.languagetool,
      dependencies.lsp4j,
      dependencies.decline,
      dependencies.scalatest, 
    ),
    assembly / assemblyJarName := s"${name.value}",
    assembly / assemblyMergeStrategy  := {
     case "META-INF/org/languagetool/language-module.properties" => MergeStrategy.concat
     case PathList("META-INF", xs @ _*) => MergeStrategy.discard
     case "module-info.class"           => MergeStrategy.discard
     case "about.html"                  => MergeStrategy.discard
     case "LICENSE"                     => MergeStrategy.discard
     case x => MergeStrategy.singleOrError
    },
    assembly / assemblyPrependShellScript := Some(defaultUniversalScript(shebang = false)),
)
