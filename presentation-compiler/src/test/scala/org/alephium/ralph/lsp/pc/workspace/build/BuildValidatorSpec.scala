package org.alephium.ralph.lsp.pc.workspace.build

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.TryValues.convertTryToSuccessOrFailure

import java.nio.file.Files

class BuildValidatorSpec extends AnyWordSpec with Matchers {

  "BuildValidator" should {
    "normalize path" in {
      val config1 = RalphcConfig.defaultParsedConfig.copy(
        contractPath = "contracts",
        artifactPath = "artifacts"
      )

      val config2 = config1.copy(
        contractPath = "./contracts",
        artifactPath = "./artifacts"
      )

      val workspacePath = Files.createTempDirectory("root_workspace")

      Files.createDirectory(workspacePath.resolve(config1.contractPath))
      Files.createDirectory(workspacePath.resolve(config1.artifactPath))

      val buildPath = workspacePath.resolve(WorkspaceBuild.BUILD_FILE_NAME)
      val actualBuildPath = RalphcConfig.persist(workspacePath, config1).success.value

      actualBuildPath shouldBe buildPath

      val parsed1 = BuildState.BuildParsed(
        buildURI = buildPath.toUri,
        code = "",
        config = config1
      )

      val parsed2 = parsed1.copy(
        config = config2
      )

      BuildValidator.validate(parsed1) shouldBe BuildValidator.validate(parsed2)
    }
  }
}
