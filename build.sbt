lazy val `presentation-compiler` =
  project
    .settings(
      scalaVersion := Versions.scala213,
      libraryDependencies ++=
        Seq(
          "org.eclipse.lsp4j" % "org.eclipse.lsp4j" % Versions.lsp4j,
          "org.alephium" %% "alephium-ralph" % Versions.ralph,
          "org.scalatest" %% "scalatest" % "3.2.16" % Test,
          "org.scalatestplus" %% "scalacheck-1-17" % "3.2.16.0" % Test
        )
    )

lazy val `lsp-server` =
  project
    .dependsOn(`presentation-compiler`)
    .settings(
      scalaVersion := Versions.scala213,
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
          "org.eclipse.lsp4j" % "org.eclipse.lsp4j" % Versions.lsp4j
        )
    )
