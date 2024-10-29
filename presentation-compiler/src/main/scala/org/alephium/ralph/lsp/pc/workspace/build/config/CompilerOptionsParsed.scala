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

package org.alephium.ralph.lsp.pc.workspace.build.config

import org.alephium.ralph.CompilerOptions
import org.alephium.ralph.lsp.pc.workspace.build.typescript.TSConfig

object CompilerOptionsParsed {

  /**
   * Converts existing values from [[CompilerOptions]] to the corresponding fields in [[CompilerOptionsParsed]].
   *
   * @param options The [[CompilerOptions]] instance to be converted.
   * @return A [[CompilerOptionsParsed]] instance containing the corresponding values.
   */
  def from(options: CompilerOptions): CompilerOptionsParsed =
    CompilerOptionsParsed(
      ignoreUnusedConstantsWarnings = Some(options.ignoreUnusedConstantsWarnings),
      ignoreUnusedVariablesWarnings = Some(options.ignoreUnusedVariablesWarnings),
      ignoreUnusedFieldsWarnings = Some(options.ignoreUnusedFieldsWarnings),
      ignoreUnusedPrivateFunctionsWarnings = Some(options.ignoreUnusedPrivateFunctionsWarnings),
      ignoreUpdateFieldsCheckWarnings = Some(options.ignoreUpdateFieldsCheckWarnings),
      ignoreCheckExternalCallerWarnings = Some(options.ignoreCheckExternalCallerWarnings),
      ignoreUnusedFunctionReturnWarnings = Some(options.ignoreUnusedFunctionReturnWarnings),
      skipAbstractContractCheck = Some(options.skipAbstractContractCheck)
    )

  /**
   * Converts TypeScript compiler options defined in `alephium.config.ts` to `ralph.json`'s [[CompilerOptionsParsed]].
   *
   * @param options The [[TSConfig.CompilerOptions]] instance to be converted.
   * @return A [[CompilerOptionsParsed]] instance containing the corresponding values.
   */
  def from(options: TSConfig.CompilerOptions): CompilerOptionsParsed =
    CompilerOptionsParsed(
      ignoreUnusedConstantsWarnings = options.ignoreUnusedConstantsWarnings,
      ignoreUnusedVariablesWarnings = options.ignoreUnusedVariablesWarnings,
      ignoreUnusedFieldsWarnings = options.ignoreUnusedFieldsWarnings,
      ignoreUnusedPrivateFunctionsWarnings = options.ignoreUnusedPrivateFunctionsWarnings,
      ignoreUpdateFieldsCheckWarnings = options.ignoreUpdateFieldsCheckWarnings,
      ignoreCheckExternalCallerWarnings = options.ignoreCheckExternalCallerWarnings,
      ignoreUnusedFunctionReturnWarnings = options.ignoreUnusedFunctionReturnWarnings,
      skipAbstractContractCheck = options.skipAbstractContractCheck
    )

}

/**
 * Represents the `compilerOptions` settings that configured in the `ralph.json` file.
 */
case class CompilerOptionsParsed(
    ignoreUnusedConstantsWarnings: Option[Boolean] = None,
    ignoreUnusedVariablesWarnings: Option[Boolean] = None,
    ignoreUnusedFieldsWarnings: Option[Boolean] = None,
    ignoreUnusedPrivateFunctionsWarnings: Option[Boolean] = None,
    ignoreUpdateFieldsCheckWarnings: Option[Boolean] = None,
    ignoreCheckExternalCallerWarnings: Option[Boolean] = None,
    ignoreUnusedFunctionReturnWarnings: Option[Boolean] = None,
    skipAbstractContractCheck: Option[Boolean] = None) {

  /**
   * Converts this [[CompilerOptionsParsed]] instance to a [[CompilerOptions]] instance.
   * Fields that are not configured (i.e. None) will be filled with default values from [[CompilerOptions.Default]].
   *
   * @return A [[CompilerOptions]] instance with the values from this [[CompilerOptionsParsed]] or default values.
   */
  def toCompilerOptions(): CompilerOptions =
    CompilerOptions(
      ignoreUnusedConstantsWarnings = ignoreUnusedConstantsWarnings getOrElse CompilerOptions.Default.ignoreUnusedConstantsWarnings,
      ignoreUnusedVariablesWarnings = ignoreUnusedVariablesWarnings getOrElse CompilerOptions.Default.ignoreUnusedVariablesWarnings,
      ignoreUnusedFieldsWarnings = ignoreUnusedFieldsWarnings getOrElse CompilerOptions.Default.ignoreUnusedFieldsWarnings,
      ignoreUnusedPrivateFunctionsWarnings = ignoreUnusedPrivateFunctionsWarnings getOrElse CompilerOptions.Default.ignoreUnusedPrivateFunctionsWarnings,
      ignoreUpdateFieldsCheckWarnings = ignoreUpdateFieldsCheckWarnings getOrElse CompilerOptions.Default.ignoreUpdateFieldsCheckWarnings,
      ignoreCheckExternalCallerWarnings = ignoreCheckExternalCallerWarnings getOrElse CompilerOptions.Default.ignoreCheckExternalCallerWarnings,
      ignoreUnusedFunctionReturnWarnings = ignoreUnusedFunctionReturnWarnings getOrElse CompilerOptions.Default.ignoreUnusedFunctionReturnWarnings,
      skipAbstractContractCheck = skipAbstractContractCheck getOrElse CompilerOptions.Default.skipAbstractContractCheck
    )

}
