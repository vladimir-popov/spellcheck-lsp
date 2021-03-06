import sbtassembly.AssemblyPlugin.defaultUniversalScript

ThisBuild / organization := "ru.dokwork"
ThisBuild / scalaVersion := "3.1.0"

ThisBuild / semanticdbEnabled                              := true
ThisBuild / semanticdbVersion                              := scalafixSemanticdb.revision
ThisBuild / scalafixDependencies += "com.github.liancheng" %% "organize-imports" % "0.6.0"

ThisBuild / githubWorkflowPublishTargetBranches := Seq()

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
    val languagetool    = "5.6"
    val lsp4j           = "0.12.0"
    val logback         = "1.2.10"
    val `scala-logging` = "3.9.4"
    val scalatest       = "3.2.10"
  }

  val languagetool    = ("org.languagetool"           % "language-en"       % versions.languagetool)
    .exclude("com.intellij", "annotations")
  val lsp4j           = "org.eclipse.lsp4j"           % "org.eclipse.lsp4j" % versions.lsp4j
  val logback         = "ch.qos.logback"              % "logback-classic"   % versions.logback
  val `scala-logging` = "com.typesafe.scala-logging" %% "scala-logging"     % versions.`scala-logging`
  val scalatest       = "org.scalatest"              %% "scalatest"         % versions.scalatest
}

lazy val `spellcheck-lsp` = (project in file("."))
  .settings(
    scalacOptions ++= compilerOptions,
    libraryDependencies ++= Seq(
      dependencies.languagetool,
      dependencies.lsp4j,
      dependencies.logback,
      dependencies.`scala-logging`,
      dependencies.scalatest % "test"
    ),
    assembly / assemblyJarName            := s"${name.value}",
    assembly / assemblyMergeStrategy      := {
      case "META-INF/org/languagetool/language-module.properties" => MergeStrategy.concat
      case PathList("META-INF", xs @ _*)                          => MergeStrategy.discard
      case "module-info.class"                                    => MergeStrategy.discard
      case "about.html"                                           => MergeStrategy.discard
      case "LICENSE"                                              => MergeStrategy.discard
      case x                                                      => MergeStrategy.singleOrError
    },
    assembly / assemblyPrependShellScript := Some(defaultUniversalScript(shebang = false))
  )
