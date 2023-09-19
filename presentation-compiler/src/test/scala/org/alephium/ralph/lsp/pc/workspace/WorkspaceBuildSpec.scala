package org.alephium.ralph.lsp.pc.workspace

import org.alephium.ralph.lsp.compiler.error.StringMessage
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.TryValues._
import org.scalatest.EitherValues._

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
      val actual = WorkspaceBuild.parseConfig(build_ralph).success.value

      actual shouldBe expected
    }
  }

  "readBuild" should {
    "report missing build file" in {
      val dir = Files.createTempDirectory("no_build_file").resolve(WorkspaceBuild.FILE_NAME).toUri

      val actual = WorkspaceBuild.readBuild(dir).left.value
      val expected = StringMessage(WorkspaceBuild.buildNotFound())

      actual shouldBe expected
    }

    "persist & read valid build file" in {
      val workspacePath = Files.createTempDirectory("workspace_URI")
      val config = WorkspaceBuild.defaultRalphcConfig

      // Persist the default config for a workspace
      val expectedBuildPath = workspacePath.resolve(WorkspaceBuild.FILE_NAME)
      val actualFilePath = WorkspaceBuild.persistConfig(workspacePath, config).success.value
      actualFilePath shouldBe expectedBuildPath

      // Read the persisted config file
      val readConfig = WorkspaceBuild.readBuild(expectedBuildPath.toUri).value
      val expectedCode = WorkspaceBuild.writeConfig(WorkspaceBuild.defaultRalphcConfig)

      readConfig shouldBe
        WorkspaceBuild(
          buildURI = expectedBuildPath.toUri,
          code = expectedCode,
          config = config
        )
    }
  }
}
