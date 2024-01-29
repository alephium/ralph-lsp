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
    .settings(
      scalaVersion := Version.scala213,
      scalacOptions += "-Xmixin-force-forwarders:false", //Duplicate RPC method initialized.
      assembly / mainClass := Some("org.alephium.ralph.lsp.Main"),
      assembly / assemblyOutputPath := file("plugin-vscode/out/ralph-lsp.jar"),
      assemblyMergeStrategy := {
        case PathList("module-info.class")        => MergeStrategy.discard
        case x if x.endsWith("module-info.class") => MergeStrategy.discard
        case other                                => assemblyMergeStrategy.value(other)
      },
      Test / scalacOptions += "-Xmixin-force-forwarders:true", // reset to default for tests.
      libraryDependencies ++=
        Seq(
          Dependencies.lsp4j,
          Dependencies.scalaTest,
          Dependencies.scalaCheck,
          Dependencies.scalaMock,
          Dependencies.logback,
          Dependencies.scalaLogging
        )
    ).dependsOn(`presentation-compiler`)

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
