package org.alephium.ralph.lsp.pc.workspace.build

import org.alephium.ralph.lsp.pc.workspace.build.error.ErrorBuildFileNotFound
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.EitherValues._

import java.net.URI
import java.nio.file.Files
import scala.collection.immutable.ArraySeq

class WorkspaceBuildSpec extends AnyWordSpec with Matchers {

  "parse" should {
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
          |  "contractPath": "contracts",
          |  "artifactPath": "artifacts"
          |}
          |""".stripMargin

      val expected = RalphcConfig.defaultParsedConfig
      val actual = RalphcConfig.parse(URI.create(""), build_ralph).value

      actual shouldBe expected
    }
  }

  "parseAndCompile" should {
    "report missing build file" in {
      val dir = Files.createTempDirectory("no_build_file").resolve(WorkspaceBuild.BUILD_FILE_NAME).toUri

      val actual = WorkspaceBuild.parseAndCompile(dir)

      val expected =
        BuildState.BuildErrored(
          buildURI = dir,
          code = None,
          errors = ArraySeq(ErrorBuildFileNotFound)
        )

      actual shouldBe expected
    }
  }

}
