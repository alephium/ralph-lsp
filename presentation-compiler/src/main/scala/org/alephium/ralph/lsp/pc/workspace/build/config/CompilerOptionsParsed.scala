// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

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
      skipAbstractContractCheck = Some(options.skipAbstractContractCheck),
      skipTests = Some(options.skipTests)
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
      skipAbstractContractCheck = options.skipAbstractContractCheck,
      skipTests = options.skipTests
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
    skipAbstractContractCheck: Option[Boolean] = None,
    skipTests: Option[Boolean] = None) {

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
      skipAbstractContractCheck = skipAbstractContractCheck getOrElse CompilerOptions.Default.skipAbstractContractCheck,
      skipTests = skipTests getOrElse CompilerOptions.Default.skipTests
    )

}
