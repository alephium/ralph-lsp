package org.alephium.ralph.lsp.pc.workspace.build

import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.client.TestClientLogger
import org.alephium.ralph.lsp.pc.log.ClientLogger
import org.alephium.ralph.lsp.pc.workspace.build.dependency.Dependency
import org.alephium.ralph.lsp.pc.workspace.{TestWorkspace, WorkspaceState}
import org.alephium.ralphc.Config
import org.scalatest.EitherValues._
import org.scalatest.OptionValues._
import org.scalatest.TryValues.convertTryToSuccessOrFailure
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.net.URI
import java.nio.file.{Files, Paths}

class RalphcConfigSpec extends AnyWordSpec with Matchers {

  implicit val clientLogger: ClientLogger =
    TestClientLogger

  "parse" should {
    "parse workspace build file" when {
      "dependencyPath is provided" in {
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
            |  "artifactPath": "artifacts",
            |  "dependencyPath": "dependencies"
            |}
            |""".stripMargin

        val expected = RalphcConfig.defaultParsedConfig.copy(dependencyPath = Some("dependencies"))
        val actual = RalphcConfig.parse(URI.create(""), build_ralph).value

        actual shouldBe expected
      }

      "dependencyPath is not provided" in {
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

        val expected = RalphcConfig.defaultParsedConfig.copy(dependencyPath = None)
        val actual = RalphcConfig.parse(URI.create(""), build_ralph).value

        actual shouldBe expected
      }
    }
  }

  "persist & read valid ralphc config" when {
    implicit val file: FileAccess =
      FileAccess.disk

    implicit val compiler: CompilerAccess =
      CompilerAccess.ralphc

    // create workspace structure with config file
    def doTest(dependencyPath: Option[String]) = {
      val config = RalphcConfig.defaultParsedConfig.copy(dependencyPath = dependencyPath)

      val workspacePath = Files.createTempDirectory("root_workspace")

      Files.createDirectory(workspacePath.resolve(config.contractPath))
      Files.createDirectory(workspacePath.resolve(config.artifactPath))
      // create only if the dependencyPath is provided by the user i.e. is in the parsed config
      // otherwise expect the dependency compiler to write to the default dependencyPath
      config.dependencyPath.foreach(dependencyPath => Files.createDirectory(workspacePath.resolve(dependencyPath)))

      // Persist the default config to the workspace
      val expectedBuildPath = workspacePath.resolve(Build.BUILD_FILE_NAME)
      val actualBuildPath = RalphcConfig.persist(workspacePath, config).success.value
      actualBuildPath shouldBe expectedBuildPath

      // Parse and compile the config file on disk
      val readConfig =
        Build.parseAndCompile(
          buildURI = expectedBuildPath.toUri,
          currentBuild = None
        )

      // The code of the parsed config (user inputted) is expected, not the compiled config's code.
      val expectedCode =
        RalphcConfig.write(config)

      // Compiled config always contains absolute paths, unlike the parsed config
      val expectedCompiledConfig =
        Config(
          compilerOptions = config.compilerOptions,
          contractPath = Paths.get(workspacePath.resolve(config.contractPath).toUri),
          artifactPath = Paths.get(workspacePath.resolve(config.artifactPath).toUri)
        )

      val parsedBuild =
        BuildState.BuildParsed(
          buildURI = expectedBuildPath.toUri,
          code = RalphcConfig.write(config),
          config = config
        )

      val compiledStd =
        Dependency
          .compile(
            parsed = parsedBuild,
            currentBuild = None
          ).asInstanceOf[BuildState.BuildCompiled]

      compiledStd.dependency shouldBe defined

      val expectedDependenciesPath =
        config.dependencyPath match {
          case Some(dependencyPath) =>
            Paths.get(workspacePath.resolve(dependencyPath).toUri)

          case None =>
            Dependency.defaultPath().value
        }

      readConfig shouldBe
        BuildState.BuildCompiled(
          buildURI = expectedBuildPath.toUri,
          code = expectedCode,
          dependency = compiledStd.dependency,
          dependencyPath = expectedDependenciesPath,
          config = expectedCompiledConfig
        )

      TestWorkspace delete WorkspaceState.Created(workspacePath.toUri)
    }

    "dependencyPath is not provided" in {
      doTest(dependencyPath = None)
    }

    "dependencyPath is provided" in {
      doTest(dependencyPath = Some("dependencies"))
    }
  }
}
