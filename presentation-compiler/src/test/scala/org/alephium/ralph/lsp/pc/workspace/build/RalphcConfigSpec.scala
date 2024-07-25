// Copyright 2024 The Alephium Authors
// This file is part of the alephium project.
//
// The library is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// The library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with the library. If not, see http://www.gnu.org/licenses/.

package org.alephium.ralph.lsp.pc.workspace.build

import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.client.TestClientLogger
import org.alephium.ralph.lsp.pc.log.ClientLogger
import org.alephium.ralph.lsp.pc.workspace.build.dependency.Dependency
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
    "parse workspace build file" when {
      "dependencyPath is provided" in {
        val build_ralph =
          """{
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
        val actual   = RalphcConfig.parse(URI.create(""), build_ralph).value

        actual shouldBe expected
      }

      "dependencyPath is not provided" in {
        val build_ralph =
          """{
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
        val actual   = RalphcConfig.parse(URI.create(""), build_ralph).value

        actual shouldBe expected
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

    // create workspace structure with config file
    def doTest(dependencyPath: Option[String]) = {
      val config = RalphcConfig.defaultParsedConfig.copy(dependencyPath = dependencyPath)

      val workspacePath = Files.createTempDirectory("root_workspace")

      Files.createDirectory(workspacePath.resolve(config.contractPath))
      Files.createDirectory(workspacePath.resolve(config.artifactPath))
      Files.createDirectory(Build.toBuildDir(workspacePath))
      // create only if the dependencyPath is provided by the user i.e. is in the parsed config
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
        BuildState.Parsed(
          buildURI = expectedBuildPath.toUri,
          code = expectedCode,
          config = config
        )

      val compiledStd =
        Dependency
          .compile(
            parsed = parsedBuild,
            currentBuild = None
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

      readConfig shouldBe
        BuildState.Compiled(
          dependencies = compiledStd.dependencies,
          dependencyPath = expectedDependenciesPath,
          config = expectedCompiledConfig,
          parsed = parsedBuild
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
