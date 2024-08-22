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
import org.alephium.ralph.lsp.pc.workspace.build.TestRalphc
import org.alephium.ralph.lsp.pc.workspace.build.config.RalphcConfigState

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class TSBuildTransformerSpec extends AnyWordSpec with Matchers with ScalaCheckDrivenPropertyChecks {

  "extract TS config" when {
    "alephium.config.ts is standard" in {
      forAll(TestTSBuildFile.genTSConfig) {
        config =>
          val tsConfigFile = TestTSBuildFile.genTSBuildFile(config).sample.get
          val extracted    = TSBuildTransformer.extractTSConfig(tsConfigFile)

          extracted shouldBe config
      }
    }

    "config is defined outside `Configuration`" in {
      val config =
        s""""|sourceDir: 'contracts',
             |artifactDir: 'artifacts'
             |const configuration: Configuration<Setting> = {}""".stripMargin

      val extracted = TSBuildTransformer.extractTSConfig(config)

      extracted.sourceDir shouldBe Some("contracts")
      extracted.artifactDir shouldBe Some("artifacts")

    }

    "directory is the empty string" in {
      val config =
        s""""|sourceDir: '',
             |artifactDir: ''""".stripMargin

      val extracted = TSBuildTransformer.extractTSConfig(config)

      extracted.sourceDir shouldBe Some("")
      extracted.artifactDir shouldBe Some("")

    }

    "config is using variable outside `Configuration`" in {
      val config =
        s""""|sourceDir: 'contracts',
             |artifactDir: 'artifacts'
             |
             |const configuration: Configuration<Setting> = {
             |  sourceDir: sourceDir,
             |  artifactDir: artifactDir
             |}""".stripMargin

      val extracted = TSBuildTransformer.extractTSConfig(config)

      extracted.sourceDir shouldBe Some("contracts")
      extracted.artifactDir shouldBe Some("artifacts")

    }

    "sourceDir and artifactDir are defined multiple time (last one taken)" in {
      val config =
        s""""|const sourceDir: 'contracts',
             |const artifactDir: 'artifacts'
             |
             |const configuration: Configuration<Setting> = {
             |  sourceDir: 'newContracts',
             |  artifactDir: 'newArtifacts'
             |}""".stripMargin

      val extracted = TSBuildTransformer.extractTSConfig(config)

      extracted.sourceDir shouldBe Some("newContracts")
      extracted.artifactDir shouldBe Some("newArtifacts")
      extracted.compilerOptions shouldBe None
    }

    "using double quotes" in {
      val config =
        s""""|const configuration: Configuration<Setting> = {
             |  sourceDir: "contracts",
             |  artifactDir: "artifacts"
             |}""".stripMargin

      val extracted = TSBuildTransformer.extractTSConfig(config)

      extracted.sourceDir shouldBe Some("contracts")
      extracted.artifactDir shouldBe Some("artifacts")
      extracted.compilerOptions shouldBe None
    }
  }

  "extract empty TS config" when {
    "config file is empty" in {
      val extracted = TSBuildTransformer.extractTSConfig("")

      extracted shouldBe TSConfig.empty
    }

    "const configuration is empty" in {
      val config = s"const configuration: Configuration<Setting> = {}"

      val extracted = TSBuildTransformer.extractTSConfig(config)

      extracted shouldBe TSConfig.empty
    }

    "config use env variable" in {
      val config =
        s""""|const configuration: Configuration<Setting> = {
             |  sourceDir: process.env.SOURCE_DIR,
             |  artifactDir: process.env.ARTIFACT_DIR,
             |  compilerOptions: {
             |    ignoreUnusedConstantsWarnings: process.env.IGNORE_UNUSED_CONSTANTS_WARNINGS,
             |    ignoreUnusedVariablesWarnings: process.env.IGNORE_UNUSED_VARIABLES_WARNINGS,
             |  }
             |}""".stripMargin

      val extracted = TSBuildTransformer.extractTSConfig(config)

      extracted shouldBe TSConfig.empty.copy(compilerOptions = Some(TSConfig.CompilerOptions.empty))
    }

    "config use any variable" in {
      val config =
        s""""|const dir = 'contracts'
             |const option = true
             |
             |const configuration: Configuration<Setting> = {
             |  sourceDir: dir,
             |  compilerOptions: {
             |    ignoreUnusedConstantsWarnings: option
             |  }
             |}""".stripMargin

      val extracted = TSBuildTransformer.extractTSConfig(config)

      extracted shouldBe TSConfig.empty.copy(compilerOptions = Some(TSConfig.CompilerOptions.empty))
    }

    "config is imported from elsewhere" in {
      val config =
        s""""|import config from './config/default';
             |export default config;""".stripMargin

      val extracted = TSBuildTransformer.extractTSConfig(config)

      extracted shouldBe TSConfig.empty
    }

    "quotes surround mismatch" in {
      val config =
        s""""|const configuration: Configuration<Setting> = {
             |  sourceDir: 'contract",
             |  artifactDir: "artifact',
             |}""".stripMargin

      val extracted = TSBuildTransformer.extractTSConfig(config)

      extracted shouldBe TSConfig.empty
    }
  }

  "merge configs" when {
    "ts config is empty" in {
      forAll(TestRalphc.genRalphcParsedConfig()) {
        ralphcConfig =>
          val merged = TSBuildTransformer.mergeConfigs(ralphcConfig, TSConfig.empty)

          merged shouldBe ralphcConfig
      }
    }

    "ts config is defined" in {
      forAll(TestTSBuildFile.genTSConfig, TestRalphc.genRalphcParsedConfig()) {
        case (tsConfig, ralphcConfig) =>
          val merged = TSBuildTransformer.mergeConfigs(ralphcConfig, tsConfig)

          merged.contractPath shouldBe tsConfig.sourceDir.getOrElse(ralphcConfig.contractPath)
          merged.artifactPath shouldBe tsConfig.artifactDir.orElse(ralphcConfig.artifactPath)
          merged.dependencyPath shouldBe ralphcConfig.dependencyPath

          tsConfig.compilerOptions match {
            case None => merged.compilerOptions shouldBe ralphcConfig.compilerOptions
            case Some(tsOptions) =>
              merged
                .compilerOptions
                .ignoreUnusedConstantsWarnings shouldBe tsOptions.ignoreUnusedConstantsWarnings.getOrElse(ralphcConfig.compilerOptions.ignoreUnusedConstantsWarnings)

              merged
                .compilerOptions
                .ignoreUnusedVariablesWarnings shouldBe tsOptions.ignoreUnusedVariablesWarnings.getOrElse(ralphcConfig.compilerOptions.ignoreUnusedVariablesWarnings)

              merged.compilerOptions.ignoreUnusedFieldsWarnings shouldBe tsOptions.ignoreUnusedFieldsWarnings.getOrElse(ralphcConfig.compilerOptions.ignoreUnusedFieldsWarnings)

              merged.compilerOptions.ignoreUnusedPrivateFunctionsWarnings shouldBe tsOptions
                .ignoreUnusedPrivateFunctionsWarnings
                .getOrElse(ralphcConfig.compilerOptions.ignoreUnusedPrivateFunctionsWarnings)

              merged
                .compilerOptions
                .ignoreUpdateFieldsCheckWarnings shouldBe tsOptions.ignoreUpdateFieldsCheckWarnings.getOrElse(ralphcConfig.compilerOptions.ignoreUpdateFieldsCheckWarnings)

              merged
                .compilerOptions
                .ignoreCheckExternalCallerWarnings shouldBe tsOptions.ignoreCheckExternalCallerWarnings.getOrElse(ralphcConfig.compilerOptions.ignoreCheckExternalCallerWarnings)
          }

      }
    }
  }

  "toRalphcParsedConfig should fall back to default config" when {
    "ts and ralph configs are empty" in {
      TSBuildTransformer.toRalphcParsedConfig(
        tsBuildURI = TestFile.genFileURI().sample.get,
        tsBuildCode = "",
        currentConfig = None
      ) shouldBe Right(RalphcConfigState.Parsed.default)
    }
  }

}
