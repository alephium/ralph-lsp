// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.workspace.build.config

import org.alephium.ralph.CompilerOptions
import org.alephium.ralph.lsp.TestFile
import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.client.TestClientLogger
import org.alephium.ralph.lsp.utils.log.ClientLogger
import org.alephium.ralph.lsp.pc.workspace.build.{Build, BuildState}
import org.alephium.ralph.lsp.pc.workspace.build.dependency.Dependency
import org.alephium.ralph.lsp.pc.workspace.build.dependency.downloader.DependencyDownloader
import org.alephium.ralph.lsp.pc.workspace.build.error.ErrorEmptyBuildFile
import org.alephium.ralph.lsp.pc.workspace.{WorkspaceState, TestWorkspace}
import org.alephium.ralphc.Config
import org.scalatest.EitherValues._
import org.scalatest.OptionValues._
import org.scalatest.TryValues.convertTryToSuccessOrFailure
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.net.URI
import java.nio.file.{Paths, Files}

class RalphcConfigSpec extends AnyWordSpec with Matchers {

  implicit val clientLogger: ClientLogger =
    TestClientLogger

  "parse" should {
    "succeed" when {
      "dependencyPath" when {
        "is provided" in {
          val build_ralph =
            """{
              |  "compilerOptions": {
              |    "ignoreUnusedConstantsWarnings": false,
              |    "ignoreUnusedVariablesWarnings": false,
              |    "ignoreUnusedFieldsWarnings": false,
              |    "ignoreUnusedPrivateFunctionsWarnings": false,
              |    "ignoreUpdateFieldsCheckWarnings": false,
              |    "ignoreCheckExternalCallerWarnings": false,
              |    "ignoreUnusedFunctionReturnWarnings": false,
              |    "skipAbstractContractCheck": false,
              |    "skipTests": false
              |  },
              |  "contractPath": "contracts",
              |  "artifactPath": "artifacts",
              |  "dependencyPath": "dependencies"
              |}
              |""".stripMargin

          val expected =
            RalphcConfigState.Parsed(
              contractPath = "contracts",
              compilerOptions = Some(CompilerOptionsParsed.from(CompilerOptions.Default)),
              artifactPath = Some("artifacts"),
              dependencyPath = Some("dependencies")
            )

          val actual = RalphcConfig.parse(URI.create(""), build_ralph).value

          actual shouldBe expected
        }

        "is not provided" in {
          val build_ralph =
            """{
              |  "compilerOptions": {
              |    "ignoreUnusedConstantsWarnings": true,
              |    "ignoreUnusedVariablesWarnings": false,
              |    "ignoreUnusedFieldsWarnings": true,
              |    "ignoreUpdateFieldsCheckWarnings": true,
              |    "ignoreUnusedFunctionReturnWarnings": false,
              |    "skipAbstractContractCheck": true
              |  },
              |  "contractPath": "contracts",
              |  "artifactPath": "artifacts"
              |}
              |""".stripMargin

          val expected =
            RalphcConfigState.Parsed(
              contractPath = "contracts",
              compilerOptions = Some(
                CompilerOptionsParsed(
                  ignoreUnusedConstantsWarnings = Some(true),
                  ignoreUnusedVariablesWarnings = Some(false),
                  ignoreUnusedFieldsWarnings = Some(true),
                  ignoreUnusedPrivateFunctionsWarnings = None, // Not configured
                  ignoreUpdateFieldsCheckWarnings = Some(true),
                  ignoreCheckExternalCallerWarnings = None, // Not configured
                  ignoreUnusedFunctionReturnWarnings = Some(false),
                  skipAbstractContractCheck = Some(true),
                  skipTests = None
                )
              ),
              artifactPath = Some("artifacts"),
              dependencyPath = None
            )
          val actual = RalphcConfig.parse(URI.create(""), build_ralph).value

          actual shouldBe expected
        }
      }

      "artifactPath" when {
        "is provided" in {
          val build_ralph =
            """{
              |  "contractPath": "contracts",
              |  "artifactPath": "artifacts"
              |}
              |""".stripMargin

          val expected = RalphcConfigState.Parsed.default.copy(contractPath = "contracts", artifactPath = Some("artifacts"))
          val actual   = RalphcConfig.parse(URI.create(""), build_ralph).value

          actual shouldBe expected
        }

        "is not provided" in {
          val build_ralph =
            """{
              |  "contractPath": ""
              |}
              |""".stripMargin

          val expected = RalphcConfigState.Parsed.default
          val actual   = RalphcConfig.parse(URI.create(""), build_ralph).value

          actual shouldBe expected
        }
      }
    }

    "fail" when {
      "build file is empty" in {
        val build_ralph =
          """
            |
            |""".stripMargin

        val fileURI = URI.create(Build.FILE_NAME)
        val actual  = RalphcConfig.parse(fileURI, build_ralph).left.value

        actual shouldBe ErrorEmptyBuildFile(fileURI)
      }
    }
  }

  "persist & read valid ralphc config" when {
    implicit val file: FileAccess =
      FileAccess.disk

    implicit val compiler: CompilerAccess =
      CompilerAccess.ralphc

    // create workspace structure with a config file
    def doTest(
        dependencyPath: Option[String],
        artifactPath: Option[String]) = {
      val config =
        RalphcConfigState
          .Parsed
          .default
          .copy(
            contractPath = "contracts",
            artifactPath = artifactPath,
            dependencyPath = dependencyPath
          )

      val workspacePath = Files.createTempDirectory("root_workspace")

      TestFile.createDirectories(workspacePath.resolve(config.contractPath))

      config
        .artifactPath
        .foreach {
          artifactPath =>
            Files.createDirectory(workspacePath.resolve(artifactPath))
        }

      Files.createDirectory(Build.toBuildDir(workspacePath))
      // create only if the dependencyPath is provided by the dev i.e. is in the parsed config
      // otherwise expect the dependency compiler to write to the default dependencyPath
      config
        .dependencyPath
        .foreach {
          dependencyPath =>
            Files.createDirectory(workspacePath.resolve(dependencyPath))
        }

      // Persist the default config to the workspace
      val expectedBuildPath = Build.toBuildFile(workspacePath)
      val actualBuildPath   = RalphcConfig.persist(workspacePath, config).success.value
      actualBuildPath shouldBe expectedBuildPath

      // Parse and compile the config file on disk
      val actualBuild =
        Build.parseAndCompile(
          buildURI = expectedBuildPath.toUri,
          currentBuild = None,
          dependencyDownloaders = DependencyDownloader.natives()
        )

      // The code of the parsed config (user inputted) is expected, not the compiled config's code.
      val expectedCode =
        RalphcConfig.write(config)

      // Compiled config always contains absolute paths, unlike the parsed config
      val expectedCompiledConfig =
        RalphcConfigState.Compiled(
          isArtifactsPathDefinedInBuild = artifactPath.isDefined,
          config = Config(
            compilerOptions = config.toRalphcCompilerOptions(),
            contractPath = Paths.get(workspacePath.resolve(config.contractPath).toUri),
            // The defined `artifactPath` or-else defaults to `contractPath`
            artifactPath = Paths.get(workspacePath.resolve(artifactPath getOrElse config.contractPath).toUri)
          )
        )

      val parsedBuild =
        BuildState.Parsed(
          buildURI = expectedBuildPath.toUri,
          code = expectedCode,
          config = config
        )

      val compiledStd =
        Dependency
          .compile(
            parsed = parsedBuild,
            currentBuild = None,
            downloaders = DependencyDownloader.natives()
          )
          .asInstanceOf[BuildState.Compiled]

      compiledStd.dependencies should have size 2

      val expectedDependenciesPath =
        config.dependencyPath match {
          case Some(dependencyPath) =>
            Paths.get(workspacePath.resolve(dependencyPath).toUri)

          case None =>
            Dependency.defaultPath().value
        }

      val expectedBuild =
        BuildState.Compiled(
          dependencies = compiledStd.dependencies,
          dependencyPath = expectedDependenciesPath,
          config = expectedCompiledConfig,
          parsed = parsedBuild
        )

      actualBuild shouldBe expectedBuild

      TestWorkspace delete WorkspaceState.Created(workspacePath.toUri)
    }

    "Optional paths are defined as" when {
      "(None, None): None are provided" in {
        doTest(dependencyPath = None, artifactPath = None)
      }

      "(Some, None): Only dependencyPath is provided" in {
        doTest(dependencyPath = Some("dependencies"), artifactPath = None)
      }

      "(None, Some): Only artifactPath is provided" in {
        doTest(dependencyPath = None, artifactPath = Some("artifacts"))
      }

      "(Some, Some): Both are provided" in {
        doTest(dependencyPath = Some("dependencies"), artifactPath = Some("artifacts"))
      }
    }
  }

}
