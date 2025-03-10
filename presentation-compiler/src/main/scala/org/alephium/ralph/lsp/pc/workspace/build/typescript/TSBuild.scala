// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.workspace.build.typescript

import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.SourceIndex
import org.alephium.ralph.lsp.pc.workspace.build.config.{RalphcConfigState, RalphcConfig}
import org.alephium.ralph.lsp.pc.workspace.build.{Build, BuildState}

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
   * Steps: read -> transform -> persist
   *
   * @param code         Current code of the TypeScript `alephium.config.ts` file.
   * @param currentBuild Current build state.
   * @param file         Provides file-IO API.
   * @return Values:
   *          - [[Left]] if build errors occurred in the TypeScript `alephium.config.ts` file.
   *          - [[None]] if no change occurred.
   *          - [[BuildState.Parsed]] the newly persisted config.
   */
  def build(
      code: Option[String],
      currentBuild: BuildState.IsCompiled
    )(implicit file: FileAccess): Either[TSBuildState.Errored, Option[BuildState.Parsed]] =
    // Step 1: Read `alephium.config.ts`'s file content
    read(
      tsBuildURI = currentBuild.tsBuildURI,
      code = code
    ) flatMap {
      case Some(tsCode) =>
        // Fetch the current `ralph.json`'s parsed config from current build
        val currentConfig =
          Build
            .getParsedOrNone(currentBuild)
            .map(_.config)

        // Step 2: Transform
        TSBuildTransformer
          .toRalphcParsedConfig(
            currentBuild.tsBuildURI,
            tsBuildCode = tsCode,
            currentConfig = currentConfig
          )
          .flatMap {
            newConfig =>
              // Step 3: Persist
              persist(
                jsonBuildURI = currentBuild.buildURI,
                currentConfig = currentConfig,
                tsBuildURI = currentBuild.tsBuildURI,
                tsBuildCode = tsCode,
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
   * Persists the input configuration [[RalphcConfigState.Parsed]] to a file specified by `buildURI`
   * only if the input configuration is different from the existing configuration.
   *
   * @param jsonBuildURI  Current build's `ralph.json` file URI.
   * @param currentConfig Current build's `ralph.json` parsed type.
   * @param tsBuildURI    Current build's `alephium.config.ts` file URI.
   * @param tsBuildCode   Current build's `alephium.config.ts` content.
   * @param updatedConfig The newly transformed configuration to persist.
   * @param file          Provides file-IO API.
   * @return Values:
   *          - [[Left]] if an error occurred in case of an IO error.
   *          - [[None]] if no change occurred.
   *          - [[BuildState.Parsed]] the newly persisted config.
   */
  def persist(
      jsonBuildURI: URI,
      currentConfig: Option[RalphcConfigState.Parsed],
      tsBuildURI: URI,
      tsBuildCode: String,
      updatedConfig: RalphcConfigState.Parsed
    )(implicit file: FileAccess): Either[TSBuildState.Errored, Option[BuildState.Parsed]] =
    if (!currentConfig.contains(updatedConfig)) {
      val updatedJSON =
        RalphcConfig.write(
          config = updatedConfig,
          indent = 2
        )

      file.write( // config did change! Persist the new config.
        fileURI = jsonBuildURI,
        string = updatedJSON,
        index = SourceIndex.empty
      ) match {
        case Right(_) =>
          // Return a parsed instance so both, the newly persisted JSON string and
          // its typed JSON RalphcConfig instance are known, for further re-build/re-compilation to save IO.
          val newParsedConfig =
            BuildState.Parsed(
              buildURI = jsonBuildURI,
              code = updatedJSON,
              config = updatedConfig
            )

          Right(Some(newParsedConfig))

        case Left(error) =>
          val tsState =
            TSBuildState.Errored(
              buildURI = tsBuildURI,
              code = Some(tsBuildCode),
              errors = ArraySeq(error)
            )

          Left(tsState)
      }
    } else {
      Right(None)
    }

}
