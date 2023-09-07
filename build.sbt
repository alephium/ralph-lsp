lazy val `ralphc-access` =
  project
    .settings(
      scalaVersion := Version.scala213,
      libraryDependencies ++=
        Seq(
          "org.alephium" %% "alephium-ralphc" % "2.0.0+206-cc55f429+20230825-0041-SNAPSHOT",
          "org.scalatest" %% "scalatest" % "3.2.16" % Test,
          "org.scalatestplus" %% "scalacheck-1-17" % "3.2.16.0" % Test,
          "org.scalamock" %% "scalamock" % Version.scalaMock % Test
        )
    )

lazy val `presentation-compiler` =
  project
    .settings(
      scalaVersion := Version.scala213,
      libraryDependencies ++=
        Seq(
          "com.outr" %% "scribe" % Version.scribe,
          "com.outr" %% "scribe-file" % Version.scribe,
          "org.scalatest" %% "scalatest" % "3.2.16" % Test,
          "org.scalatestplus" %% "scalacheck-1-17" % "3.2.16.0" % Test,
          "org.scalamock" %% "scalamock" % Version.scalaMock % Test
        )
    ).dependsOn(`ralphc-access`)

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
          "org.eclipse.lsp4j" % "org.eclipse.lsp4j" % Version.lsp4j,
          "org.scalatest" %% "scalatest" % "3.2.16" % Test,
          "org.scalatestplus" %% "scalacheck-1-17" % "3.2.16.0" % Test,
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
