// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.workspace.build.typescript

/*
 * case class to store the TypeScript build configuration
 * obtained from the `alephium.config.ts` file using regex.
 */
case class TSConfig(
    sourceDir: Option[String],
    artifactDir: Option[String],
    compilerOptions: Option[TSConfig.CompilerOptions])

object TSConfig {

  def empty: TSConfig = TSConfig(None, None, None)

  case class CompilerOptions(
      ignoreUnusedConstantsWarnings: Option[Boolean],
      ignoreUnusedVariablesWarnings: Option[Boolean],
      ignoreUnusedFieldsWarnings: Option[Boolean],
      ignoreUnusedPrivateFunctionsWarnings: Option[Boolean],
      ignoreUpdateFieldsCheckWarnings: Option[Boolean],
      ignoreCheckExternalCallerWarnings: Option[Boolean],
      ignoreUnusedFunctionReturnWarnings: Option[Boolean],
      skipAbstractContractCheck: Option[Boolean],
      errorOnWarnings: Option[Boolean] = None)

  object CompilerOptions {

    def empty: CompilerOptions = CompilerOptions(None, None, None, None, None, None, None, None)

  }

}
