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
import org.alephium.ralph.lsp.pc.workspace.build.{RalphcConfig, BuildState}

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
   * @param code         Current code of the TypeScript `alephium.config.ts` file.
   * @param currentBuild Current build state.
   *                     This function is invoked if the current `ralph.json`
   *                     file is currently errored or not, therefore, the state is [[BuildState.IsCompiled]].
   * @param file         Provides file-IO API.
   * @return The next build state.
   */
  def build(
      code: Option[String],
      currentBuild: BuildState.IsCompiled
    )(implicit file: FileAccess): Option[BuildState.Errored] =
    compile(
      code = code,
      currentBuild = currentBuild
    ) match {
      case Left(tsError) =>
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
              errored.copy(tsState = Some(tsError))
          }

        Some(newState)

      case Right(_) =>
        None
    }

  /**
   * Compiles the TypeScript `alephium.config.ts` file.
   *
   * @param code         Current code of the TypeScript `alephium.config.ts` file.
   *                     If `None`, this will be read from disk.
   * @param currentBuild Current build state.
   *                     This function is invoked if the current `ralph.json`
   *                     file is currently errored or not, therefore, the state is [[BuildState.IsCompiled]].
   * @param file         Provides file-IO API.
   * @return Either the errored state or the path to the compiled file.
   */
  private def compile(
      code: Option[String],
      currentBuild: BuildState.IsCompiled
    )(implicit file: FileAccess): Either[TSBuildState.Errored, Path] =
    for {
      // Read the TypeScript code from disk
      code <-
        read(
          tsBuildURI = currentBuild.tsBuildURI,
          code = code
        )

      // transform TypeScript code to ralph.json object
      config <-
        TSBuildTransformer.toRalphcParsedConfig(
          tsBuildCode = code,
          currentBuild = currentBuild
        )

      // persist the ralph.json object
      path <-
        persist(
          buildURI = currentBuild.buildURI,
          tsBuildURI = currentBuild.tsBuildURI,
          config = config
        )
    } yield path

  /**
   * Reads the content of the TypeScript `alephium.config.ts` file.
   *
   * @param tsBuildURI The URI of the TypeScript `alephium.config.ts` build file.
   * @param code       Optional code content. If `None`, the content will be read from disk.
   * @param file       Provides file-IO API.
   * @return Either the errored state reporting any file-IO errors or the content of the file as a string.
   */
  private def read(
      tsBuildURI: URI,
      code: Option[String]
    )(implicit file: FileAccess): Either[TSBuildState.Errored, String] =
    code match {
      case Some(code) =>
        Right(code)

      case None =>
        file
          .read(tsBuildURI)
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
   * Persists the input configuration [[RalphcConfig.RalphcParsedConfig]] to a file specified by `buildURI`.
   *
   * @param buildURI   The URI of the build `ralph.json` file.
   * @param tsBuildURI The URI of the TypeScript `alephium.config.ts` build file.
   * @param config     The parsed configuration to persist.
   * @param file       Provides file-IO API.
   * @return Either the errored state in-case of IO error or the path to the persisted file path.
   */
  private def persist(
      buildURI: URI,
      tsBuildURI: URI,
      config: RalphcConfig.RalphcParsedConfig
    )(implicit file: FileAccess): Either[TSBuildState.Errored, Path] =
    file
      .write(
        fileURI = buildURI,
        string = RalphcConfig.write(config, indent = 2),
        index = SourceIndex.empty
      )
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
