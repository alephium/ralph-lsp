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

import scala.collection.immutable.ArraySeq
import org.alephium.ralph.{SourceIndex, CompilerOptions}
import org.alephium.ralph.lsp.pc.workspace.build.{RalphcConfig, BuildState}
import org.alephium.ralph.lsp.access.compiler.message.error.StringError

import scala.util.Random

object TSBuildTransformer {

  /**
   * Transforms the content of the TypeScript `alephium.config.ts` build file
   * to JSON `ralph.json` build file of type [[RalphcConfig.RalphcParsedConfig]].
   *
   * @param tsBuildCode  Content of the TypeScript `alephium.config.ts` file.
   * @param currentBuild Current build state.
   *                     This function is invoked whether the current `ralph.json`
   *                     file is currently errored or not, therefore, the state is [[BuildState.IsCompiled]].
   * @return Either the errored state of the `alephium.config.ts` build file or the new `ralph.json` build file to persist.
   */
  def toRalphcParsedConfig(
      tsBuildCode: String,
      currentBuild: BuildState.IsCompiled): Either[TSBuildState.Errored, RalphcConfig.RalphcParsedConfig] =
    // FOR DEMO ONLY: Randomly generate a `ralph.json` file.
    if (Random.nextBoolean())
      Right(genRandomRalphcParsedConfig())
    else // FOR DEMO ONLY: Randomly generate an error to report in `alephium.config.ts` file.
      Left(
        TSBuildState.Errored(
          buildURI = currentBuild.tsBuildURI,
          code = Some(tsBuildCode),
          errors = ArraySeq(StringError("Test dummy error", SourceIndex.empty))
        )
      )

  /** FOR DEMO ONLY: Randomly generate a `ralph.json` file. */
  private def genRandomRalphcParsedConfig(): RalphcConfig.RalphcParsedConfig =
    RalphcConfig
      .defaultParsedConfig
      .copy(
        compilerOptions = CompilerOptions(
          ignoreUnusedConstantsWarnings = Random.nextBoolean(),
          ignoreUnusedVariablesWarnings = Random.nextBoolean(),
          ignoreUnusedFieldsWarnings = Random.nextBoolean(),
          ignoreUpdateFieldsCheckWarnings = Random.nextBoolean(),
          ignoreUnusedPrivateFunctionsWarnings = Random.nextBoolean(),
          ignoreCheckExternalCallerWarnings = Random.nextBoolean()
        ),
        contractPath = Random.shuffle(List(Random.alphanumeric.take(10).mkString, "contracts")).head,
        artifactPath = Random.shuffle(List(Random.alphanumeric.take(10).mkString, "artifacts")).head
      )

}
