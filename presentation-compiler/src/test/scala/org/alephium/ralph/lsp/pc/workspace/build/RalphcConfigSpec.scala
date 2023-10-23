package org.alephium.ralph.lsp.pc.workspace.build

import org.alephium.ralphc.Config
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.TryValues.convertTryToSuccessOrFailure

import java.nio.file.{Files, Paths}

class RalphcConfigSpec extends AnyWordSpec with Matchers {

  "persist & read valid ralphc config" in {
    // create workspace structure with config file
    val config = RalphcConfig.defaultParsedConfig
    val workspacePath = Files.createTempDirectory("root_workspace")
    Files.createDirectory(workspacePath.resolve(config.contractPath))
    Files.createDirectory(workspacePath.resolve(config.artifactPath))
    // Persist the default config to the workspace
    val expectedBuildPath = workspacePath.resolve(WorkspaceBuild.BUILD_FILE_NAME)
    val actualBuildPath = RalphcConfig.persist(workspacePath, config).success.value
    actualBuildPath shouldBe expectedBuildPath

    // Parse and compile the config file on disk
    val readConfig = WorkspaceBuild.parseAndCompile(expectedBuildPath.toUri)

    // The parsed code is the expected code, not the compiled.
    val expectedCode =
      RalphcConfig.write(config)

    // Compiled config contains absolute paths unlike parsed config
    val expectedCompiledConfig =
      Config(
        compilerOptions = config.compilerOptions,
        contractPath = Paths.get(workspacePath.resolve(config.contractPath).toUri),
        artifactPath = Paths.get(workspacePath.resolve(config.artifactPath).toUri)
      )

    readConfig shouldBe
      BuildState.BuildCompiled(
        buildURI = expectedBuildPath.toUri,
        code = expectedCode,
        config = expectedCompiledConfig,
        dependencies = BuildDependencies.empty
      )
  }
}
