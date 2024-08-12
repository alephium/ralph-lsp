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

package org.alephium.ralph.lsp.pc.workspace.build.dependency.downloader

import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage
import org.alephium.ralph.lsp.pc.log.{ClientLogger, StrictImplicitLogging}
import org.alephium.ralph.lsp.pc.workspace.WorkspaceState
import org.alephium.ralph.lsp.pc.workspace.build.config.{RalphcConfigState, RalphcConfig}
import org.alephium.ralph.lsp.pc.workspace.build.error.ErrorEmptyErrorsOnDownload
import org.alephium.ralph.lsp.pc.workspace.build.{Build, BuildState}
import org.alephium.ralph.{SourceIndex, CompilerOptions}

import java.nio.file.Path
import scala.collection.immutable.ArraySeq

/**
 * A dependency downloader, responsible for downloading code dependencies into an un-compiled workspace.
 */
trait DependencyDownloader extends StrictImplicitLogging { self =>

  /**
   * Downloads dependency code to an un-compiled workspace.
   *
   * @param dependencyPath The configured dependency directory.
   * @param errorIndex     Index to report downloading errors.
   * @return Either an array of downloading errors or an un-compiled workspace on successful download.
   */
  protected def _download(
      dependencyPath: Path,
      errorIndex: SourceIndex
    )(implicit logger: ClientLogger): Either[ArraySeq[CompilerMessage.AnyError], WorkspaceState.UnCompiled]

  /**
   * Invokes the function [[_download]], checking that errors are non-empty.
   *
   * @param dependencyPath The configured dependency directory.
   * @param errorIndex     Index to report downloading errors.
   * @return Either an array of downloading errors or an un-compiled workspace on successful download.
   */
  final def download(
      dependencyPath: Path,
      errorIndex: SourceIndex
    )(implicit logger: ClientLogger): Either[ArraySeq[CompilerMessage.AnyError], WorkspaceState.UnCompiled] =
    _download(
      dependencyPath = dependencyPath,
      errorIndex = errorIndex
    ) match {
      case left @ Left(errors) =>
        // Log when the downloader results in empty errors.
        // This is to detect scenarios where compilation stalls due to a bug, leading to no error reported to the editor.
        // This log should help in tracing that bug.
        if (errors.isEmpty) {
          val error = ErrorEmptyErrorsOnDownload(self, errorIndex)
          logger.error(error.message)
          Left(ArraySeq(error))
        } else {
          left
        }

      case right @ Right(_) =>
        right
    }

}

object DependencyDownloader {

  /** All dependency downloaders */
  def all(): ArraySeq[DependencyDownloader] =
    ArraySeq(
      StdInterfaceDownloader,
      BuiltInFunctionDownloader
    )

  /**
   * Currently dependencies do not contain a `ralph.json` file.
   * This function create a default file.
   *
   * @param workspaceDir The directory of the workspace for which this build file is being created.
   * @return Compiled build state representing the default `ralph.json`.
   */
  def defaultBuild(workspaceDir: Path): BuildState.Compiled = {
    val buildDir =
      Build.toBuildFile(workspaceDir)

    // Create a config with the workspace directory as the only directory.
    // Sets`contractPath` as the workspace directory.
    val parsedConfig =
      RalphcConfigState.Parsed(
        compilerOptions = CompilerOptions.Default,
        contractPath = workspaceDir.toString,
        artifactPath = None
      )

    val json =
      RalphcConfig.write(parsedConfig)

    val parsed =
      BuildState.Parsed(
        buildURI = buildDir.toUri,
        code = json,
        config = parsedConfig
      )

    // a compiled config
    val compiledConfig =
      RalphcConfigState.Compiled(
        isArtifactsPathDefinedInBuild = parsedConfig.artifactPath.isDefined,
        config = org
          .alephium
          .ralphc
          .Config(
            compilerOptions = CompilerOptions.Default,
            contractPath = workspaceDir,
            artifactPath = workspaceDir
          )
      )

    BuildState.Compiled(
      dependencies = ArraySeq.empty,
      dependencyPath = workspaceDir,
      config = compiledConfig,
      parsed = parsed
    )
  }

}
