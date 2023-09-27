lazy val `compiler-access` =
  project
    .settings(
      scalaVersion := Version.scala213,
      libraryDependencies ++=
        Seq(
          Dependencies.ralphc,
          Dependencies.scalaTest,
          Dependencies.scalaCheck,
          Dependencies.logback,
          Dependencies.scalaLogging
        )
    )

lazy val `presentation-compiler` =
  project
    .settings(
      scalaVersion := Version.scala213,
      libraryDependencies ++=
        Seq(
          Dependencies.scalaTest,
          Dependencies.scalaCheck,
          Dependencies.scalaMock,
          Dependencies.logback,
          Dependencies.scalaLogging
        )
    ).dependsOn(`compiler-access`)

lazy val `lsp-server` =
  project
    .dependsOn(`presentation-compiler`)
    .settings(
      scalaVersion := Version.scala213,
      scalacOptions += "-Xmixin-force-forwarders:false", //Duplicate RPC method initialized.
      assembly / mainClass := Some("org.alephium.ralph.lsp.Main"),
      assembly / assemblyJarName := "ralph-lsp.jar",
      assemblyMergeStrategy := {
        case PathList("module-info.class")        => MergeStrategy.discard
        case x if x.endsWith("module-info.class") => MergeStrategy.discard
        case other                                => assemblyMergeStrategy.value(other)
      },
      libraryDependencies ++=
        Seq(
          Dependencies.lsp4j,
          Dependencies.scalaTest,
          Dependencies.scalaCheck,
          Dependencies.scalaMock,
          Dependencies.logback,
          Dependencies.scalaLogging
        )
    )

lazy val `plugin-intellij` =
  project
    .enablePlugins(SbtIdeaPlugin)
    .settings(
      version := "0.0.1-SNAPSHOT",
      scalaVersion := Version.scala213,
      ThisBuild / intellijPluginName := "Ralph LSP",
      ThisBuild / intellijBuild := "232.9559-EAP-CANDIDATE-SNAPSHOT",
      ThisBuild / intellijPlatform := IntelliJPlatform.IdeaUltimate,
      packageMethod := PackagingMethod.Standalone(),
      Global / intellijAttachSources := true,
      Compile / javacOptions ++= "--release" :: "17" :: Nil,
      intellijPlugins += "com.intellij.properties".toPlugin,
      libraryDependencies += "com.eclipsesource.minimal-json" % "minimal-json" % "0.9.5" withSources(),
      Compile / unmanagedResourceDirectories += baseDirectory.value / "resources",
      Test / unmanagedResourceDirectories += baseDirectory.value / "testResources"
    )
