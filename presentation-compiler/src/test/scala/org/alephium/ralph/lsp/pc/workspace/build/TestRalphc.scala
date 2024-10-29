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

import org.alephium.ralph.CompilerOptions
import org.alephium.ralph.lsp.TestCommon.genName
import org.alephium.ralph.lsp.pc.workspace.build.config.{RalphcConfigState, CompilerOptionsParsed}
import org.scalacheck.{Gen, Arbitrary}

/** Ralph compiler related test functions */
object TestRalphc {

  def genCompilerOptions(): Gen[CompilerOptions] =
    for {
      ignoreUnusedConstantsWarnings        <- Arbitrary.arbitrary[Boolean]
      ignoreUnusedVariablesWarnings        <- Arbitrary.arbitrary[Boolean]
      ignoreUnusedFieldsWarnings           <- Arbitrary.arbitrary[Boolean]
      ignoreUnusedPrivateFunctionsWarnings <- Arbitrary.arbitrary[Boolean]
      ignoreUpdateFieldsCheckWarnings      <- Arbitrary.arbitrary[Boolean]
      ignoreCheckExternalCallerWarnings    <- Arbitrary.arbitrary[Boolean]
      ignoreUnusedFunctionReturnWarnings   <- Arbitrary.arbitrary[Boolean]
      skipAbstractContractCheck            <- Arbitrary.arbitrary[Boolean]
    } yield CompilerOptions(
      ignoreUnusedConstantsWarnings = ignoreUnusedConstantsWarnings,
      ignoreUnusedVariablesWarnings = ignoreUnusedVariablesWarnings,
      ignoreUnusedFieldsWarnings = ignoreUnusedFieldsWarnings,
      ignoreUnusedPrivateFunctionsWarnings = ignoreUnusedPrivateFunctionsWarnings,
      ignoreUpdateFieldsCheckWarnings = ignoreUpdateFieldsCheckWarnings,
      ignoreCheckExternalCallerWarnings = ignoreCheckExternalCallerWarnings,
      ignoreUnusedFunctionReturnWarnings = ignoreUnusedFunctionReturnWarnings,
      skipAbstractContractCheck = skipAbstractContractCheck
    )

  def genCompilerOptionsJSON(): Gen[CompilerOptionsParsed] =
    for {
      ignoreUnusedConstantsWarnings        <- Gen.option(Arbitrary.arbitrary[Boolean])
      ignoreUnusedVariablesWarnings        <- Gen.option(Arbitrary.arbitrary[Boolean])
      ignoreUnusedFieldsWarnings           <- Gen.option(Arbitrary.arbitrary[Boolean])
      ignoreUnusedPrivateFunctionsWarnings <- Gen.option(Arbitrary.arbitrary[Boolean])
      ignoreUpdateFieldsCheckWarnings      <- Gen.option(Arbitrary.arbitrary[Boolean])
      ignoreCheckExternalCallerWarnings    <- Gen.option(Arbitrary.arbitrary[Boolean])
      ignoreUnusedFunctionReturnWarnings   <- Gen.option(Arbitrary.arbitrary[Boolean])
      skipAbstractContractCheck            <- Gen.option(Arbitrary.arbitrary[Boolean])
    } yield CompilerOptionsParsed(
      ignoreUnusedConstantsWarnings = ignoreUnusedConstantsWarnings,
      ignoreUnusedVariablesWarnings = ignoreUnusedVariablesWarnings,
      ignoreUnusedFieldsWarnings = ignoreUnusedFieldsWarnings,
      ignoreUnusedPrivateFunctionsWarnings = ignoreUnusedPrivateFunctionsWarnings,
      ignoreUpdateFieldsCheckWarnings = ignoreUpdateFieldsCheckWarnings,
      ignoreCheckExternalCallerWarnings = ignoreCheckExternalCallerWarnings,
      ignoreUnusedFunctionReturnWarnings = ignoreUnusedFunctionReturnWarnings,
      skipAbstractContractCheck = skipAbstractContractCheck
    )

  def genRalphcParsedConfig(
      compilerOptionsJSON: Gen[Option[CompilerOptionsParsed]] = Gen.option(genCompilerOptionsJSON()),
      contractsFolderName: Gen[String] = genName,
      artifactsFolderName: Gen[Option[String]] = Gen.option(genName),
      dependenciesFolderName: Gen[Option[String]] = Gen.option(genName)): Gen[RalphcConfigState.Parsed] =
    for {
      contractsFolderName    <- contractsFolderName
      compilerOptionsJSON    <- compilerOptionsJSON
      artifactsFolderName    <- artifactsFolderName
      dependenciesFolderName <- dependenciesFolderName
    } yield RalphcConfigState.Parsed(
      contractPath = contractsFolderName,
      compilerOptions = compilerOptionsJSON,
      artifactPath = artifactsFolderName,
      dependencyPath = dependenciesFolderName
    )

}
