// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.workspace.build.typescript

import org.alephium.ralph.lsp.TestFile
import org.alephium.ralph.lsp.pc.workspace.build.TestRalphc
import org.alephium.ralph.lsp.pc.workspace.build.config.{RalphcConfigState, CompilerOptionsParsed}
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalatest.EitherValues._

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
      forAll(Gen.option(TestRalphc.genRalphcParsedConfig())) {
        jsonConfig =>
          val actual =
            TSBuildTransformer.mergeConfigs(
              jsonConfig = jsonConfig,
              tsConfig = TSConfig.empty
            )

          val expected =
            RalphcConfigState.Parsed(
              contractPath = TSBuildTransformer.DEFAULT_SOURCE_DIR,
              artifactPath = None,
              dependencyPath = jsonConfig.flatMap(_.dependencyPath),
              compilerOptions = None
            )

          actual shouldBe expected
      }
    }

    "ts config is defined" in {
      forAll(TestTSBuildFile.genTSConfig, TestRalphc.genRalphcParsedConfig()) {
        case (tsConfig, jsonConfig) =>
          val merged = TSBuildTransformer.mergeConfigs(Some(jsonConfig), tsConfig)

          val expectedContractPath =
            tsConfig.sourceDir getOrElse TSBuildTransformer.DEFAULT_SOURCE_DIR

          val expectedCompilerOptions =
            tsConfig.compilerOptions map CompilerOptionsParsed.from

          val expected =
            RalphcConfigState.Parsed(
              contractPath = expectedContractPath,
              artifactPath = None,
              // dependencyPath remain unchanged.
              // changes in `alephium.config.ts` should be change `dependencyPath`.
              dependencyPath = jsonConfig.dependencyPath,
              compilerOptions = expectedCompilerOptions
            )

          merged shouldBe expected
      }

    }
  }

  "toRalphcParsedConfig should fall back to default config" when {
    "ts and ralph configs are empty" in {
      val actual =
        TSBuildTransformer
          .toRalphcParsedConfig(
            tsBuildURI = TestFile.genFileURI().sample.get,
            tsBuildCode = "",
            currentConfig = None
          )
          .value

      val expected =
        RalphcConfigState
          .Parsed
          .default
          .copy(contractPath = TSBuildTransformer.DEFAULT_SOURCE_DIR)

      actual shouldBe expected
    }
  }

}
