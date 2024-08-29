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

package org.alephium.ralph.lsp.pc.workspace.build.typescript

import org.alephium.ralph.lsp.TestFile
import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.client.TestClientLogger
import org.alephium.ralph.lsp.pc.log.ClientLogger
import org.alephium.ralph.lsp.pc.workspace.build.{TestRalphc, BuildState, TestBuild}
import org.alephium.ralph.lsp.pc.workspace.build.config.{RalphcConfigState, RalphcConfig, CompilerOptionsParsed}
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.OptionValues._
import org.scalatest.EitherValues._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.util.Random

/** Test-cases for the function [[TSBuild.build]] */
class TSBuildBuildSpec extends AnyWordSpec with Matchers with ScalaCheckDrivenPropertyChecks {

  implicit val clientLogger: ClientLogger = TestClientLogger
  implicit val file: FileAccess           = FileAccess.disk
  implicit val compiler: CompilerAccess   = CompilerAccess.ralphc

  "ignore execution" when {
    "`alephium.config.ts` file does not exist" in {
      forAll(TestBuild.genCompiledOK()) {
        jsonBuild =>
          // run the build
          val actualJSONBuild =
            TSBuild
              .build(
                code = None,
                currentBuild = jsonBuild
              )
              .value

          // empty = no change occurred
          actualJSONBuild shouldBe empty

          /**
           * Test: The JSON persisted on disk remains unchanged
           */
          val persistedJSON =
            TestFile readAll jsonBuild.buildURI

          val persistedConfig =
            RalphcConfig
              .parse(
                buildURI = jsonBuild.buildURI,
                json = persistedJSON
              )
              .value

          jsonBuild.parsed.config shouldBe persistedConfig

          TestBuild deleteDirectory jsonBuild
      }
    }
  }

  "not update `ralph.json`" when {
    "both build files have identical settings" in {
      forAll(TestBuild.genCompiledOK(config = TestRalphc.genRalphcParsedConfig(dependenciesFolderName = Some("some_deps")))) {
        jsonBuild =>
          // the JSON on disk before the build
          val preBuildJSON = TestFile readAll jsonBuild.buildURI

          // A TypeScript `alephium.config.ts` build file with ALL values the same as the generated `ralph.json` build.
          val tsBuild =
            TSConfig(
              sourceDir = Some(jsonBuild.parsed.config.contractPath),
              artifactDir = jsonBuild.parsed.config.artifactPath,
              compilerOptions = jsonBuild.parsed.config.compilerOptions map {
                compilerOptions =>
                  TSConfig.CompilerOptions(
                    ignoreUnusedConstantsWarnings = compilerOptions.ignoreUnusedConstantsWarnings,
                    ignoreUnusedVariablesWarnings = compilerOptions.ignoreUnusedVariablesWarnings,
                    ignoreUnusedFieldsWarnings = compilerOptions.ignoreUnusedFieldsWarnings,
                    ignoreUnusedPrivateFunctionsWarnings = compilerOptions.ignoreUnusedPrivateFunctionsWarnings,
                    ignoreUpdateFieldsCheckWarnings = compilerOptions.ignoreUpdateFieldsCheckWarnings,
                    ignoreCheckExternalCallerWarnings = compilerOptions.ignoreCheckExternalCallerWarnings,
                    ignoreUnusedFunctionReturnWarnings = compilerOptions.ignoreUnusedFunctionReturnWarnings,
                    errorOnWarnings = Gen.option(Random.nextBoolean()).sample.get
                  )
              }
            )

          // convert TSBuild to a String
          val tsBuildString =
            TestTSBuildFile
              .genTSBuildFile(tsBuild)
              .sample
              .get

          // run the build
          val actualJSONBuild =
            TSBuild
              .build(
                code = Some(tsBuildString),
                currentBuild = jsonBuild
              )(file = null) // file is null to test no file I/O occurs, i.e. on-disk `ralph.json` does not get updated
              .value

          // empty = no change occurred
          actualJSONBuild shouldBe empty

          // the JSON on disk after the build
          val postBuildJSON = TestFile readAll jsonBuild.buildURI
          // `ralph.json` remains unchanged
          preBuildJSON shouldBe postBuildJSON

          TestBuild deleteDirectory jsonBuild
      }
    }
  }

  "update `ralph.json`" when {
    "`alephium.config.ts` file updates all settings" in {
      // A TypeScript `alephium.config.ts` file with ALL values set to some value.
      // Expect all value `ralph.json` to be written or overwritten with these values.
      val tsBuild =
        TSConfig(
          sourceDir = Some("some_source"),
          artifactDir = Some("some_art"),
          compilerOptions = Some(
            TSConfig.CompilerOptions(
              ignoreUnusedConstantsWarnings = Some(true),
              ignoreUnusedVariablesWarnings = Some(true),
              ignoreUnusedFieldsWarnings = Some(true),
              ignoreUnusedPrivateFunctionsWarnings = Some(true),
              ignoreUpdateFieldsCheckWarnings = Some(true),
              ignoreCheckExternalCallerWarnings = Some(true),
              ignoreUnusedFunctionReturnWarnings = Some(true),
              errorOnWarnings = Some(true)
            )
          )
        )

      // generates a `ralph.json` build, some configs are set a
      forAll(TestBuild.genCompiledOK(config = TestRalphc.genRalphcParsedConfig(dependenciesFolderName = Some("some_deps")))) {
        jsonBuild =>
          // convert TSBuild to a String
          val tsBuildString =
            TestTSBuildFile
              .genTSBuildFile(tsBuild)
              .sample
              .get

          // persist the TSBuild
          TestFile.write(jsonBuild.tsBuildURI, tsBuildString)

          // run the build
          val actualJSONBuild =
            TSBuild
              .build(
                code = None,
                currentBuild = jsonBuild
              )
              .value
              .value

          /**
           * The following tests the expected states
           */
          // expect the `ralph.json` parsed state to include all values of the `alephium.config.ts` build file.
          val expectedJSONConfig =
            RalphcConfigState.Parsed(
              compilerOptions = Some(
                CompilerOptionsParsed(
                  ignoreUnusedConstantsWarnings = Some(tsBuild.compilerOptions.value.ignoreUnusedConstantsWarnings.value),
                  ignoreUnusedVariablesWarnings = Some(tsBuild.compilerOptions.value.ignoreUnusedVariablesWarnings.value),
                  ignoreUnusedFieldsWarnings = Some(tsBuild.compilerOptions.value.ignoreUnusedFieldsWarnings.value),
                  ignoreUnusedPrivateFunctionsWarnings = Some(tsBuild.compilerOptions.value.ignoreUnusedPrivateFunctionsWarnings.value),
                  ignoreUpdateFieldsCheckWarnings = Some(tsBuild.compilerOptions.value.ignoreUpdateFieldsCheckWarnings.value),
                  ignoreCheckExternalCallerWarnings = Some(tsBuild.compilerOptions.value.ignoreCheckExternalCallerWarnings.value),
                  ignoreUnusedFunctionReturnWarnings = Some(tsBuild.compilerOptions.value.ignoreUnusedFunctionReturnWarnings.value)
                )
              ),
              contractPath = tsBuild.sourceDir.value,
              artifactPath = Some(tsBuild.artifactDir.value),
              // dependency path is not configured in `alephium.config.ts`, so it should remain the same as the existing `ralph.json` configured value
              // tests that it does not get overwritten.
              dependencyPath = Some(jsonBuild.parsed.config.dependencyPath.value)
            )

          // The JSON expected to be persisted on disk
          val expectedJSON =
            RalphcConfig.write(
              config = expectedJSONConfig,
              indent = 2
            )

          val expectedJSONBuild =
            BuildState.Parsed(
              buildURI = jsonBuild.buildURI,
              code = expectedJSON,
              config = expectedJSONConfig
            )

          // the returned JSON build contains the updated configs
          actualJSONBuild shouldBe expectedJSONBuild

          // the JSON persisted on disk should also contain the updated configs
          val persistedRalphJSON = TestFile readAll jsonBuild.buildURI
          persistedRalphJSON shouldBe expectedJSON

          TestBuild deleteDirectory jsonBuild
      }
    }
  }

}
