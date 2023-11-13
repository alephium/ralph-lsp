package org.alephium.ralph.lsp.pc.workspace.build

import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.workspace.build.dependency.Dependency
import org.alephium.ralphc.Config
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.EitherValues._
import org.scalatest.TryValues.convertTryToSuccessOrFailure

import java.net.URI
import java.nio.file.{Files, Paths}

class RalphcConfigSpec extends AnyWordSpec with Matchers {

  "persist & read valid ralphc config" in {
    // create workspace structure with config file
    val config = RalphcConfig.defaultParsedConfig
    val workspacePath = Files.createTempDirectory("root_workspace")
    Files.createDirectory(workspacePath.resolve(config.contractPath))
    Files.createDirectory(workspacePath.resolve(config.artifactPath))
    // Persist the default config to the workspace
    val expectedBuildPath = workspacePath.resolve(Build.BUILD_FILE_NAME)
    val actualBuildPath = RalphcConfig.persist(workspacePath, config).success.value
    actualBuildPath shouldBe expectedBuildPath

    implicit val file: FileAccess =
      FileAccess.disk

    implicit val compiler: CompilerAccess =
      CompilerAccess.ralphc

    // Parse and compile the config file on disk
    val readConfig =
      Build.parseAndCompile(
        buildURI = expectedBuildPath.toUri,
        currentBuild = None
      )

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

    val parsedBuild =
      BuildState.BuildParsed(
        buildURI = expectedBuildPath.toUri,
        code = RalphcConfig.write(config),
        config = config
      )

    val compiledStd =
      Dependency
        .downloadAndCompileStd(parsed = parsedBuild)
        .asInstanceOf[BuildState.BuildCompiled]

    compiledStd.dependency shouldBe defined

    readConfig shouldBe
      BuildState.BuildCompiled(
        buildURI = expectedBuildPath.toUri,
        code = expectedCode,
        dependency = compiledStd.dependency,
        config = expectedCompiledConfig
      )
  }

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
}
