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

import com.typesafe.scalalogging.StrictLogging
import scala.collection.immutable.ArraySeq
import org.alephium.ralph.{SourceIndex, CompilerOptions}
import org.alephium.ralph.lsp.access.compiler.message.error.StringError
import org.alephium.ralph.lsp.pc.workspace.build.config.RalphcConfigState

import java.net.URI
import scala.annotation.unused
import scala.util.Random

import scala.util.matching.Regex

object TSBuildTransformer {

  case class TSConfig(
      sourceDir: Option[String],
      artifactDir: Option[String],
      ignoreUnusedConstantsWarnings: Option[Boolean],
      ignoreUnusedVariablesWarnings: Option[Boolean],
      ignoreUnusedFieldsWarnings: Option[Boolean],
      ignoreUnusedPrivateFunctionsWarnings: Option[Boolean],
      ignoreUpdateFieldsCheckWarnings: Option[Boolean],
      ignoreCheckExternalCallerWarnings: Option[Boolean],
      errorOnWarnings: Option[Boolean] = None)

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
      @unused tsBuildURI: URI,
      tsBuildCode: String,
      currentConfig: Option[RalphcConfigState.Parsed]): Either[TSBuildState.Errored, RalphcConfigState.Parsed] =
    // TODO Do we want some error if we can't extract the config?
    Right(
      mergeConfig(
        currentConfig.getOrElse(RalphcConfigState.Parsed.default),
        extractTSConfig(tsBuildCode)
      )
    )

  /**
   * Merge the TypeScript build configuration with the current Ralph configuration.
   * The TypeScript build configuration takes precedence over the Ralph configuration.
   * If a field is not present in the TypeScript build configuration, the Ralph configuration is used.
   */
  private def mergeConfig(
      parsed: RalphcConfigState.Parsed,
      tsConfig: TSConfig): RalphcConfigState.Parsed = {
    val compilerOptions = parsed.compilerOptions
    RalphcConfigState.Parsed(
      compilerOptions = CompilerOptions(
        ignoreUnusedConstantsWarnings = tsConfig.ignoreUnusedConstantsWarnings.getOrElse(compilerOptions.ignoreUnusedConstantsWarnings),
        ignoreUnusedVariablesWarnings = tsConfig.ignoreUnusedVariablesWarnings.getOrElse(compilerOptions.ignoreUnusedVariablesWarnings),
        ignoreUnusedFieldsWarnings = tsConfig.ignoreUnusedFieldsWarnings.getOrElse(compilerOptions.ignoreUnusedFieldsWarnings),
        ignoreUnusedPrivateFunctionsWarnings = tsConfig.ignoreUnusedPrivateFunctionsWarnings.getOrElse(compilerOptions.ignoreUnusedPrivateFunctionsWarnings),
        ignoreUpdateFieldsCheckWarnings = tsConfig.ignoreUpdateFieldsCheckWarnings.getOrElse(compilerOptions.ignoreUpdateFieldsCheckWarnings),
        ignoreCheckExternalCallerWarnings = tsConfig.ignoreCheckExternalCallerWarnings.getOrElse(compilerOptions.ignoreCheckExternalCallerWarnings)
      ),
      contractPath = tsConfig.sourceDir.getOrElse(parsed.contractPath),
      artifactPath = tsConfig.artifactDir.orElse(parsed.artifactPath),
      dependencyPath = parsed.dependencyPath
    )
  }

  private val sourceDirRegex              = """sourceDir\s*:\s*'([^']*)'""".r
  private val artifactDirRegex            = """artifactDir\s*:\s*'([^']*)'""".r
  private val compilerOptionsRegex: Regex = """compilerOptions\s*:\s*\{([^}]*)\}""".r

  private val optionsRegex: Regex =
    """\s*(?<key>errorOnWarnings|ignoreUnusedConstantsWarnings|ignoreUnusedVariablesWarnings|ignoreUnusedFieldsWarnings|ignoreUnusedPrivateFunctionsWarnings|ignoreUpdateFieldsCheckWarnings|ignoreCheckExternalCallerWarnings)\s*:\s*(?<value>true|false)""".r

  private def parseBooleanOption(option: Option[String]): Option[Boolean] =
    option.map(_.toBoolean)

  def extractTSConfig(configContent: String): TSConfig = {

    val compilerOptionsContent = compilerOptionsRegex.findFirstMatchIn(configContent).map(_.group(1)).getOrElse("")

    val optionsMap = optionsRegex
      .findAllMatchIn(compilerOptionsContent)
      .map {
        m =>
          m.group("key") -> m.group("value")
      }
      .toMap

    val sourceDir   = sourceDirRegex.findFirstMatchIn(configContent).map(_.group(1))
    val artifactDir = artifactDirRegex.findFirstMatchIn(configContent).map(_.group(1))

    TSConfig(
      sourceDir = sourceDir,
      artifactDir = artifactDir,
      ignoreUnusedConstantsWarnings = parseBooleanOption(optionsMap.get("ignoreUnusedConstantsWarnings")),
      ignoreUnusedVariablesWarnings = parseBooleanOption(optionsMap.get("ignoreUnusedVariablesWarnings")),
      ignoreUnusedFieldsWarnings = parseBooleanOption(optionsMap.get("ignoreUnusedFieldsWarnings")),
      ignoreUnusedPrivateFunctionsWarnings = parseBooleanOption(optionsMap.get("ignoreUnusedPrivateFunctionsWarnings")),
      ignoreUpdateFieldsCheckWarnings = parseBooleanOption(optionsMap.get("ignoreUpdateFieldsCheckWarnings")),
      ignoreCheckExternalCallerWarnings = parseBooleanOption(optionsMap.get("ignoreCheckExternalCallerWarnings")),
      errorOnWarnings = parseBooleanOption(optionsMap.get("errorOnWarnings"))
    )
  }

}
