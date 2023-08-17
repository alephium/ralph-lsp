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
