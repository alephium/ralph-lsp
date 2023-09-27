package org.alephium.ralph.lsp.pc.workspace.build

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.TryValues.convertTryToSuccessOrFailure

import java.nio.file.Files

class RalphcConfigSpec extends AnyWordSpec with Matchers {

  "persist & read valid ralphc config" in {
    val config = RalphcConfig.defaultCompiledConfig

    val workspacePath = Files.createTempDirectory("root_workspace")
    Files.createDirectory(workspacePath.resolve(config.contractPath))
    Files.createDirectory(workspacePath.resolve(config.artifactPath))

    // Persist the default config for a workspace
    val expectedBuildPath = workspacePath.resolve(WorkspaceBuild.BUILD_FILE_NAME)
    val actualFilePath = RalphcConfig.persist(workspacePath, config).success.value
    actualFilePath shouldBe expectedBuildPath

    // Read the persisted config file
    val readConfig = WorkspaceBuild.parseAndCompile(expectedBuildPath.toUri)
    val expectedCode = RalphcConfig.write(RalphcConfig.defaultCompiledConfig)

    readConfig shouldBe
      BuildState.BuildCompiled(
        buildURI = expectedBuildPath.toUri,
        code = expectedCode,
        config = config
      )
  }
}
