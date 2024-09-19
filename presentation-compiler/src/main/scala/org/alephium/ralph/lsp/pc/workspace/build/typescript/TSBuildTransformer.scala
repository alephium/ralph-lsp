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

import org.alephium.ralph.lsp.pc.workspace.build.config.{RalphcConfigState, CompilerOptionsParsed}

import java.net.URI
import scala.annotation.unused
import scala.util.matching.Regex

object TSBuildTransformer {

  /** The default `sourceDir` directory as documented in <a href="https://docs.alephium.org/sdk/cli/#configuration">Configuration</a> */
  val DEFAULT_SOURCE_DIR = "contracts"

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
    // TODO Do we want to report some error to `alephium.config.ts` if we can't extract the config? currently we always fall back to the current or default config
    Right(
      mergeConfigs(
        jsonConfig = currentConfig,
        tsConfig = extractTSConfig(tsBuildCode)
      )
    )

  /**
   * Merge the TypeScript build configuration with the current Ralph configuration.
   * The TypeScript build configuration takes precedence over the Ralph configuration.
   * If a field is not present or couldn't be extracted in the TypeScript build configuration, the Ralph configuration is used.
   *
   * @param jsonConfig Current Ralph configuration.
   * @param tsConfig   TypeScript build configuration.
   * @return The merged Ralph configuration.
   */
  def mergeConfigs(
      jsonConfig: Option[RalphcConfigState.Parsed],
      tsConfig: TSConfig): RalphcConfigState.Parsed =
    RalphcConfigState.Parsed(
      contractPath = tsConfig.sourceDir getOrElse DEFAULT_SOURCE_DIR,
      // Currently, artifactPath is not used until #84 is implemented, so its is always defaulted to None
      artifactPath = None,
      // leave dependencyPath in `ralph.json` unchanged.
      dependencyPath = jsonConfig.flatMap(_.dependencyPath),
      compilerOptions = tsConfig.compilerOptions map CompilerOptionsParsed.from
    )

  /**
   * TS String can be surrounded by either single or double quotes
   * Unfortunately, scala regex does not support lookbehind, so we match the quotes
   * and then check latter if they are the same
   *
   * Some other engine would support `\1` : sourceDir\s*:\s*(['"])([^\1]*)\1
   *
   * TODO: `sourceDir` and `artifactDir` are search in the whole file, should we limit the search inside `const configuration: Configuration` ?
   *       The problem is that the `configuration` object can be defined in multiple ways, someone could use: `const conf = {}`
   *       We could maybe first search the exported name by: `export default  xxx` and then search the configuration object
   */

  private val sourceDirRegex   = """sourceDir\s*:\s*(['"])([^['"]]*)(['"])""".r
  private val artifactDirRegex = """artifactDir\s*:\s*(['"])([^['"]]*)(['"])""".r

  private val compilerOptionsRegex = """compilerOptions\s*:\s*\{([^}]*)\}""".r

  private val optionNames: List[String] =
    List(
      "ignoreUnusedConstantsWarnings",
      "ignoreUnusedVariablesWarnings",
      "ignoreUnusedFieldsWarnings",
      "ignoreUnusedPrivateFunctionsWarnings",
      "ignoreUpdateFieldsCheckWarnings",
      "ignoreCheckExternalCallerWarnings",
      "ignoreUnusedFunctionReturnWarnings",
      "errorOnWarnings"
    )

  private val optionsRegex: Regex =
    s"""\\s*(?<key>${optionNames.mkString("|")})\\s*:\\s*(?<value>true|false)""".r

  private def parseBooleanOption(option: Option[String]): Option[Boolean] =
    option.map(_.toBoolean)

  /**
   * Extracts the TypeScript build configuration from the content of the `alephium.config.ts` file.
   * The configuration is extracted using regex.
   *
   * @param tsConfigContent Content of the `alephium.config.ts` file.
   *
   * @return The extracted TypeScript build configuration.
   */
  def extractTSConfig(tsConfigContent: String): TSConfig = {

    val compilerOptionsContent = compilerOptionsRegex.findAllMatchIn(tsConfigContent).map(_.group(1)).toSeq.lastOption

    val optionsMapOpt =
      compilerOptionsContent.map {
        content =>
          optionsRegex
            .findAllMatchIn(content)
            .map(
              m => m.group("key") -> m.group("value")
            )
            .toMap
      }

    /*
     * We check the surrounding quotes to ensure it's either `".."` or `'..'`
     */
    val sourceDir = sourceDirRegex
      .findAllMatchIn(tsConfigContent)
      .flatMap {
        m =>
          if (m.groupCount != 3 || m.group(1) != m.group(3)) {
            None
          } else {
            Some(m.group(2))
          }
      }
      .toSeq
      .lastOption

    val artifactDir = artifactDirRegex
      .findAllMatchIn(tsConfigContent)
      .flatMap {
        m =>
          if (m.groupCount != 3 || m.group(1) != m.group(3)) {
            None
          } else {
            Some(m.group(2))
          }
      }
      .toSeq
      .lastOption

    TSConfig(
      sourceDir = sourceDir,
      artifactDir = artifactDir,
      compilerOptions = optionsMapOpt.map {
        optionsMap =>
          TSConfig.CompilerOptions(
            ignoreUnusedConstantsWarnings = parseBooleanOption(optionsMap.get("ignoreUnusedConstantsWarnings")),
            ignoreUnusedVariablesWarnings = parseBooleanOption(optionsMap.get("ignoreUnusedVariablesWarnings")),
            ignoreUnusedFieldsWarnings = parseBooleanOption(optionsMap.get("ignoreUnusedFieldsWarnings")),
            ignoreUnusedPrivateFunctionsWarnings = parseBooleanOption(optionsMap.get("ignoreUnusedPrivateFunctionsWarnings")),
            ignoreUpdateFieldsCheckWarnings = parseBooleanOption(optionsMap.get("ignoreUpdateFieldsCheckWarnings")),
            ignoreCheckExternalCallerWarnings = parseBooleanOption(optionsMap.get("ignoreCheckExternalCallerWarnings")),
            ignoreUnusedFunctionReturnWarnings = parseBooleanOption(optionsMap.get("ignoreUnusedFunctionReturnWarnings")),
            errorOnWarnings = parseBooleanOption(optionsMap.get("errorOnWarnings"))
          )
      }
    )
  }

}
