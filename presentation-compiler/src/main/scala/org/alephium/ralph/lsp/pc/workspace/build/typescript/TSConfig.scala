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
      errorOnWarnings: Option[Boolean] = None)

  object CompilerOptions {

    def empty: CompilerOptions = CompilerOptions(None, None, None, None, None, None, None)

  }

}
