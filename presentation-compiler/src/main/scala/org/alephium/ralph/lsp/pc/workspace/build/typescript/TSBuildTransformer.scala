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
import org.alephium.ralph.lsp.access.compiler.message.error.StringError
import org.alephium.ralph.lsp.pc.workspace.build.config.RalphcConfigState

import java.net.URI
import scala.annotation.unused
import scala.util.Random

object TSBuildTransformer {

  /**
   * Transforms the content of the TypeScript `alephium.config.ts` build file
   * to JSON `ralph.json` build file of type [[RalphcConfigState.Parsed]].
   *
   * @param tsBuildURI    URI for the `alephium.config.ts` build file.
   * @param tsBuildCode   Content of the `alephium.config.ts` file.
   * @param currentConfig Current `ralph.json` config that is known by the editor.
   *                      Note: This function is invoked whether the current `ralph.json`
   *                      build is errored or not, [[None]] indicating that the current `ralph.json`
   *                      contains errors, or it does not exist.
   * @return Either the errored state of the `alephium.config.ts` build file or the new `ralph.json` build file to persist.
   */
  def toRalphcParsedConfig(
      tsBuildURI: URI,
      tsBuildCode: String,
      @unused("for now") currentConfig: Option[RalphcConfigState.Parsed]): Either[TSBuildState.Errored, RalphcConfigState.Parsed] =
    // FOR DEMO ONLY: Randomly generate a `ralph.json` file.
    if (Random.nextBoolean())
      Right(genRandomRalphcParsedConfig())
    else // FOR DEMO ONLY: Randomly generate an error to report in `alephium.config.ts` file.
      Left(
        TSBuildState.Errored(
          buildURI = tsBuildURI,
          code = Some(tsBuildCode),
          errors = ArraySeq(StringError("Test dummy error", SourceIndex.empty))
        )
      )

  /** FOR DEMO ONLY: Randomly generate a `ralph.json` file. */
  private def genRandomRalphcParsedConfig(): RalphcConfigState.Parsed =
    RalphcConfigState
      .Parsed
      .default
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
        artifactPath = Some(Random.shuffle(List(Random.alphanumeric.take(10).mkString, "artifacts")).head)
      )

}
