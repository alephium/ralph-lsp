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

import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.SourceIndex
import org.alephium.ralph.lsp.pc.workspace.build.{RalphcConfig, Build, BuildState}

import java.net.URI
import java.nio.file.{Path, Paths}
import scala.collection.immutable.ArraySeq

object TSBuild {

  val FILE_EXTENSION = "config.ts"

  /** Alephium Build file of a workspace */
  val FILE_NAME = s"alephium.$FILE_EXTENSION"

  /**
   * Returns the file path of the build file given the workspace path.
   *
   * @param workspacePath The path of the workspace.
   * @return The file path of the build file.
   */
  def toBuildFile(workspacePath: Path): Path =
    workspacePath.resolve(FILE_NAME)

  /**
   * Returns the file URI of the build file given the workspace URI.
   *
   * @param workspaceURI The URI of the workspace.
   * @return The file URI of the build file.
   */
  def toBuildFile(workspaceURI: URI): URI =
    toBuildFile(Paths.get(workspaceURI)).toUri

  /**
   * Builds the TypeScript `alephium.config.ts` file.
   *
   * This function is invoked regardless of whether the current `ralph.json`
   * file is currently errored or not, therefore, the state is [[BuildState.IsCompiled]].
   *
   * @param code         Current code of the TypeScript `alephium.config.ts` file.
   * @param currentBuild Current build state.
   * @param file         Provides file-IO API.
   * @return Values:
   *          - [[Left]] if build errors occurred in the TypeScript `alephium.config.ts` file.
   *          - [[None]] if no change occurred.
   *          - [[RalphcConfig.RalphcParsedConfig]] the newly persisted config, which is the same
   *            as the input config.
   */
  def build(
      code: Option[String],
      currentBuild: BuildState.IsCompiled
    )(implicit file: FileAccess): Either[BuildState.Errored, Option[RalphcConfig.RalphcParsedConfig]] =
    compile(
      code = code,
      currentBuild = currentBuild
    ) match {
      case Left(tsError) =>
        // There were errors in `alephium.config.ts` file.
        // Store these errors within the workspace build's error state.
        val newState =
          currentBuild match {
            case currentBuild: BuildState.Compiled =>
              BuildState.Errored(
                buildURI = currentBuild.buildURI,
                codeOption = currentBuild.codeOption,
                errors = ArraySeq.empty,
                tsState = Some(tsError),
                dependencies = currentBuild.dependencies,
                activateWorkspace = None
              )

            case errored: BuildState.Errored =>
              // Current build state is already in error state,
              // store `alephium.config.ts` errors within it.
              errored.copy(tsState = Some(tsError))
          }

        Left(newState)

      case Right(config) =>
        Right(config)
    }

  /**
   * Compiles the TypeScript `alephium.config.ts` file.
   *
   * @param code         Current code of the TypeScript `alephium.config.ts` file.
   *                     If `None`, this will be read from disk.
   * @param currentBuild Current build state.
   * @param file         Provides file-IO API.
   * @return Values:
   *          - [[Left]] if build errors occurred in the TypeScript `alephium.config.ts` file.
   *          - [[None]] if no change occurred.
   *          - [[RalphcConfig.RalphcParsedConfig]] the newly persisted config, which is the same
   *            as the input config.
   */
  private def compile(
      code: Option[String],
      currentBuild: BuildState.IsCompiled
    )(implicit file: FileAccess): Either[TSBuildState.Errored, Option[RalphcConfig.RalphcParsedConfig]] =
    read(
      tsBuildURI = currentBuild.tsBuildURI,
      code = code
    ) flatMap {
      case Some(tsCode) =>
        TSBuildTransformer
          .toRalphcParsedConfig(
            tsBuildCode = tsCode,
            currentBuild = currentBuild
          )
          .flatMap {
            newConfig =>
              persist(
                jsonBuildURI = currentBuild.buildURI,
                jsonBuildCode = currentBuild.codeOption,
                tsBuildURI = currentBuild.tsBuildURI,
                tsBuildCode = code,
                updatedConfig = newConfig
              )
          }

      case None =>
        Right(None)
    }

  /**
   * Reads the content of the TypeScript `alephium.config.ts` file.
   *
   * @param tsBuildURI The URI of the TypeScript `alephium.config.ts` build file.
   * @param code       Optional code content. If `None`, the content will be read from disk.
   * @param file       Provides file-IO API.
   * @return Values:
   *          - [[Left]] if build or IO errors occurred in the TypeScript `alephium.config.ts` file.
   *          - [[None]] if `alephium.config.ts` file does not exist.
   *          - [[String]] content of the file.
   */
  private def read(
      tsBuildURI: URI,
      code: Option[String]
    )(implicit file: FileAccess): Either[TSBuildState.Errored, Option[String]] =
    code match {
      case someCode @ Some(_) =>
        Right(someCode)

      case None =>
        file
          .readIfExists(tsBuildURI)
          .left
          .map {
            error =>
              TSBuildState.Errored(
                buildURI = tsBuildURI,
                code = None,
                errors = ArraySeq(error)
              )
          }
    }

  /**
   * Persists the input configuration [[RalphcConfig.RalphcParsedConfig]] to a file specified by `buildURI`
   * only if the input configuration is different from the existing configuration.
   *
   * @param jsonBuildURI  Current build's `ralph.json` file URI.
   * @param jsonBuildCode Current build's `ralph.json` content.
   * @param tsBuildURI    Current build's `alephium.config.ts` file URI.
   * @param tsBuildCode   Current build's `alephium.config.ts` content.
   * @param updatedConfig The newly transformed configuration to persist.
   * @param file          Provides file-IO API.
   * @return Values:
   *          - [[Left]] if an error occurred in case of an IO error.
   *          - [[None]] if no change occurred.
   *          - [[RalphcConfig.RalphcParsedConfig]] the newly persisted config, which is the same
   *            as the input config.
   */
  def persist(
      jsonBuildURI: URI,
      jsonBuildCode: Option[String],
      tsBuildURI: URI,
      tsBuildCode: Option[String],
      updatedConfig: RalphcConfig.RalphcParsedConfig
    )(implicit file: FileAccess): Either[TSBuildState.Errored, Option[RalphcConfig.RalphcParsedConfig]] = {
    val configChanged =
      Build.parse(
        buildURI = jsonBuildURI,
        json = jsonBuildCode
      ) match {
        case existing: BuildState.Parsed =>
          updatedConfig != existing.config

        case _: BuildState.Errored =>
          // current config is in error state
          // the new config from the transformer might fix it, so allow persisting it!
          true
      }

    if (configChanged)
      file.write( // config did change! Persist the new config.
        fileURI = jsonBuildURI,
        string = RalphcConfig.write(updatedConfig, indent = 2),
        index = SourceIndex.empty
      ) match {
        case Right(_) =>
          Right(Some(updatedConfig))

        case Left(error) =>
          val tsState =
            TSBuildState.Errored(
              buildURI = tsBuildURI,
              code = tsBuildCode,
              errors = ArraySeq(error)
            )

          Left(tsState)
      }
    else
      Right(None)

  }

}
