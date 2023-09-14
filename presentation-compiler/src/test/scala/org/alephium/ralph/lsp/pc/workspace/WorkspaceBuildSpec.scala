package org.alephium.ralph.lsp.pc.workspace

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.TryValues._

import java.nio.file.Files

class WorkspaceBuildSpec extends AnyWordSpec with Matchers {

  "readConfig" should {
    "parse workspace build file" in {
      val build_ralph =
        """
          |{
          |  "compilerOptions": {
          |    "ignoreUnusedConstantsWarnings": false,
          |    "ignoreUnusedVariablesWarnings": false,
          |    "ignoreUnusedFieldsWarnings": false,
          |    "ignoreUnusedPrivateFunctionsWarnings": false,
          |    "ignoreUpdateFieldsCheckWarnings": false,
          |    "ignoreCheckExternalCallerWarnings": false
          |  },
          |  "contractPath": "./contracts",
          |  "artifactPath": "./artifacts"
          |}
          |""".stripMargin

      val expected = WorkspaceBuild.defaultRalphcConfig
      val actual = WorkspaceBuild.readConfig(build_ralph).success.value

      actual shouldBe expected
    }
  }

  "readBuild" should {
    "report missing build file" in {
      val dir = Files.createTempDirectory("no_build_file").toUri

      val actual = WorkspaceBuild.readBuild(dir).failed.get.getMessage
      val expected = WorkspaceBuild.fileNotFoundException().getMessage

      actual shouldBe expected
    }

    "persist & read valid build file" in {
      val workspacePath = Files.createTempDirectory("workspace_URI")
      val config = WorkspaceBuild.defaultRalphcConfig

      // Persist the default config for a workspace
      val expectedFilePath = workspacePath.resolve(WorkspaceBuild.FILE_NAME)
      val actualFilePath = WorkspaceBuild.persistConfig(workspacePath, config).success.value
      actualFilePath shouldBe expectedFilePath

      // Read the persisted config file
      val readConfig = WorkspaceBuild.readBuild(workspacePath.toUri).success.value
      readConfig shouldBe
        WorkspaceBuild(
          workspaceURI = workspacePath.toUri,
          config = config
        )
    }
  }
}
