package org.alephium.ralph.lsp.pc.workspace

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.TryValues._

import java.nio.file.Files

class WorkspaceConfigSpec extends AnyWordSpec with Matchers {

  "read config" should {
    "parse workspace config" in {
      val config =
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

      val expected = WorkspaceConfig.defaultRalphcConfig
      val actual = WorkspaceConfig.readConfig(config).success.value

      actual shouldBe expected
    }
  }

  "readWorkspaceConfig" should {
    "report missing config file" in {
      val dir = Files.createTempDirectory("no_config").toUri

      val actual = WorkspaceConfig.readWorkspaceConfig(dir).failed.get.getMessage
      val expected = WorkspaceConfig.fileNotFoundException().getMessage

      actual shouldBe expected
    }

    "persist & read valid config file" in {
      val workspacePath = Files.createTempDirectory("workspace_URI")
      val config = WorkspaceConfig.defaultRalphcConfig

      // Persist the default config for a workspace
      val expectedFilePath = workspacePath.resolve(WorkspaceConfig.FILE_NAME)
      val actualFilePath = WorkspaceConfig.persistConfig(workspacePath, config).success.value
      actualFilePath shouldBe expectedFilePath

      // Read the persisted config file
      val readConfig = WorkspaceConfig.readWorkspaceConfig(workspacePath.toUri).success.value
      readConfig shouldBe
        WorkspaceConfig(
          workspaceURI = workspacePath.toUri,
          ralphcConfig = config
        )
    }
  }
}
