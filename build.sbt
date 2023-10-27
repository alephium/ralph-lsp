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
        case x if x.endsWith("ralph-built-in-functions.json") => MergeStrategy.first
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

lazy val downloadWeb3AndInstallStd = taskKey[Unit]("Download alephium-web3 source code and copy std interface to the correct resource folder")

downloadWeb3AndInstallStd := {
  val web3Dir = new File(s"target/web3/alephium-web3-${Version.web3}")
  val stdDir = new File("compiler-access/src/main/resources/std")
  val web3StdDir = new File(s"target/web3/alephium-web3-${Version.web3}/packages/web3/std")

  if(java.nio.file.Files.notExists(web3Dir.toPath())) {
      IO.unzipURL(new URL(s"https://github.com/alephium/alephium-web3/archive/refs/tags/v${Version.web3}.zip"), new File("target/web3"))
  }

  IO.copyDirectory(web3StdDir, stdDir)
}

//Download and install of web3 will always be performed before compilation
Compile / compile := ((Compile / compile) dependsOn downloadWeb3AndInstallStd).value

lazy val downloadRalphVSCodeAndInstallBuiltInFunctions = taskKey[Unit]("Download ralph-vscode source code and copy built-in functions json file")

downloadRalphVSCodeAndInstallBuiltInFunctions := {
  val fileName = "ralph-built-in-functions.json"
  val dlDir = new File(s"target/ralph-vscode/ralph-vscode-${Version.ralphVSCode}")
  val sourceFile = new File(s"target/ralph-vscode/ralph-vscode-${Version.ralphVSCode}/src/provider/builtIn/$fileName")
  val targetFile = new File(s"lsp-server/src/main/resources/$fileName")

  if(java.nio.file.Files.notExists(dlDir.toPath())) {
      IO.unzipURL(new URL(s"https://github.com/alephium/ralph-vscode/archive/refs/tags/v${Version.ralphVSCode}.zip"), new File("target/ralph-vscode"))
  }

  IO.copyFile(sourceFile, targetFile)
}

Compile / compile := ((Compile / compile) dependsOn downloadRalphVSCodeAndInstallBuiltInFunctions).value
