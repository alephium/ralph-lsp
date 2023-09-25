package org.alephium.ralph.lsp.pc.workspace.build

import org.alephium.ralph.lsp.compiler.error.StringMessage
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.EitherValues._
import org.scalatest.TryValues._

import java.net.URI
import java.nio.file.Files
import scala.collection.immutable.ArraySeq

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
      val actual = WorkspaceBuild.parseConfig(URI.create(""), build_ralph).value

      actual shouldBe expected
    }
  }

  "readBuild" should {
    "report missing build file" in {
      val dir = Files.createTempDirectory("no_build_file").resolve(WorkspaceBuild.BUILD_FILE_NAME).toUri

      val actual = WorkspaceBuild.readBuild(dir)

      val expected =
        BuildState.BuildErrored(
          buildURI = dir,
          code = None,
          errors = ArraySeq(StringMessage(WorkspaceBuild.buildNotFound()))
        )

      actual shouldBe expected
    }

    "persist & read valid build file" in {
      val workspacePath = Files.createTempDirectory("workspace_URI")
      val config = WorkspaceBuild.defaultRalphcConfig

      // Persist the default config for a workspace
      val expectedBuildPath = workspacePath.resolve(WorkspaceBuild.BUILD_FILE_NAME)
      val actualFilePath = WorkspaceBuild.persistConfig(workspacePath, config).success.value
      actualFilePath shouldBe expectedBuildPath

      // Read the persisted config file
      val readConfig = WorkspaceBuild.readBuild(expectedBuildPath.toUri)
      val expectedCode = WorkspaceBuild.writeConfig(WorkspaceBuild.defaultRalphcConfig)

      readConfig shouldBe
        BuildState.BuildCompiled(
          buildURI = expectedBuildPath.toUri,
          code = expectedCode,
          config = config
        )
    }
  }
}
