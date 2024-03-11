package org.alephium.ralph.lsp.pc.workspace.build

import org.alephium.ralph.SourceIndex
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.workspace.build.error.ErrorDependencyPathIsWithinContractPath
import org.scalatest.OptionValues._
import org.scalatest.TryValues.convertTryToSuccessOrFailure
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.Files
import scala.collection.immutable.ArraySeq

class BuildValidatorSpec extends AnyWordSpec with Matchers {

  "validate" should {
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

      val buildPath = workspacePath.resolve(Build.BUILD_FILE_NAME)
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

      implicit val file: FileAccess =
        FileAccess.disk

      BuildValidator.validate(parsed1) shouldBe BuildValidator.validate(parsed2)
    }

    "Fail: contractPath and dependencyPath" when {
      def doTest(config: RalphcConfig.RalphcParsedConfig) = {
        // create a build file for the config
        val build =
          TestBuild
            .genParsed(config = config)
            .sample
            .get

        TestBuild persist build

        implicit val file: FileAccess =
          FileAccess.disk

        val actual =
          BuildValidator.validate(build)

        // errors should reported for both the field
        actual.value.errors should contain theSameElementsAs
          ArraySeq(
            // error for field dependencyPath
            ErrorDependencyPathIsWithinContractPath(SourceIndex(build.code.lastIndexOf(config.dependencyPath.value), config.dependencyPath.value.length, Some(build.buildURI))),
            // error for field contractPath
            ErrorDependencyPathIsWithinContractPath(SourceIndex(build.code.lastIndexOf(config.contractPath), config.contractPath.length, Some(build.buildURI)))
          )

        TestBuild delete build
      }

      "they are identical" in {
        val config =
          RalphcConfig.defaultParsedConfig.copy(
            contractPath = "contracts",
            artifactPath = "artifacts",
            dependencyPath = Some("contracts")
          )

        doTest(config)
      }

      "contractPath is within dependencyPath" in {
        val config =
          RalphcConfig.defaultParsedConfig.copy(
            contractPath = "dependencies/contracts",
            artifactPath = "artifacts",
            dependencyPath = Some("dependencies")
          )

        doTest(config)
      }

      "dependencyPath is within contractPath" in {
        val config =
          RalphcConfig.defaultParsedConfig.copy(
            contractPath = "contracts",
            artifactPath = "artifacts",
            dependencyPath = Some("contracts/dependencies")
          )

        doTest(config)
      }
    }

    "Pass: contractPath and dependencyPath" when {
      def doTest(config: RalphcConfig.RalphcParsedConfig) = {
        // create a build file for the config
        val build =
          TestBuild
            .genParsed(config = config)
            .sample
            .get

        TestBuild persist build

        implicit val file: FileAccess =
          FileAccess.disk

        val actual =
          BuildValidator.validate(build)

        // expect no errors
        actual shouldBe empty

        TestBuild delete build
      }

      "they are distinct" in {
        val config =
          RalphcConfig.defaultParsedConfig.copy(
            contractPath = "contracts",
            artifactPath = "artifacts",
            dependencyPath = Some("dependencies")
          )

        doTest(config)
      }

      "they are distinct within a root folder" in {
        val config =
          RalphcConfig.defaultParsedConfig.copy(
            contractPath = "my_code/contracts",
            artifactPath = "artifacts",
            dependencyPath = Some("my_code/dependencies")
          )

        doTest(config)
      }
    }
  }
}
