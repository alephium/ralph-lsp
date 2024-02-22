package org.alephium.ralph.lsp.pc.workspace.build

import org.alephium.ralph.CompilerOptions
import org.alephium.ralph.lsp.pc.workspace.build.RalphcConfig.RalphcParsedConfig
import org.alephium.ralph.lsp.TestCommon.genName
import org.scalacheck.{Arbitrary, Gen}

/** Ralph compiler related test functions */
object TestRalphc {

  def genCompilerOptions(): Gen[CompilerOptions] =
    for {
      ignoreUnusedConstantsWarnings <- Arbitrary.arbitrary[Boolean]
      ignoreUnusedVariablesWarnings <- Arbitrary.arbitrary[Boolean]
      ignoreUnusedFieldsWarnings <- Arbitrary.arbitrary[Boolean]
      ignoreUnusedPrivateFunctionsWarnings <- Arbitrary.arbitrary[Boolean]
      ignoreUpdateFieldsCheckWarnings <- Arbitrary.arbitrary[Boolean]
      ignoreCheckExternalCallerWarnings <- Arbitrary.arbitrary[Boolean]
    } yield
      CompilerOptions(
        ignoreUnusedConstantsWarnings = ignoreUnusedConstantsWarnings,
        ignoreUnusedVariablesWarnings = ignoreUnusedVariablesWarnings,
        ignoreUnusedFieldsWarnings = ignoreUnusedFieldsWarnings,
        ignoreUnusedPrivateFunctionsWarnings = ignoreUnusedPrivateFunctionsWarnings,
        ignoreUpdateFieldsCheckWarnings = ignoreUpdateFieldsCheckWarnings,
        ignoreCheckExternalCallerWarnings = ignoreCheckExternalCallerWarnings
      )

  def genRalphcParsedConfig(compilerOptions: Gen[CompilerOptions] = genCompilerOptions()): Gen[RalphcParsedConfig] =
    for {
      compilerOptions <- compilerOptions
      contractsFolderName <- genName
      artifactsFolderName <- genName
      dependenciesFolderName <- Gen.option(genName)
    } yield
      RalphcParsedConfig(
        compilerOptions = compilerOptions,
        contractPath = contractsFolderName,
        artifactPath = artifactsFolderName,
        dependencyPath = dependenciesFolderName
      )
}
