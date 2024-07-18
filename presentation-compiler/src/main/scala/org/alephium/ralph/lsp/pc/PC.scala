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

package org.alephium.ralph.lsp.pc

import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.log.ClientLogger
import org.alephium.ralph.lsp.pc.util.URIUtil
import org.alephium.ralph.lsp.pc.workspace.build.error.ErrorUnknownFileType
import org.alephium.ralph.lsp.pc.workspace.build.{Build, BuildState}
import org.alephium.ralph.lsp.pc.workspace.{WorkspaceState, WorkspaceFile, WorkspaceFileEvent, Workspace}

import java.net.URI
import scala.collection.immutable.ArraySeq

/**
 * Implements public presentation compiler APIs.
 *
 * To invoke a compilation, there are only two ways:
 *  - When a file is changed, use [[PC.changed]]
 *  - When a file is deleted or created, use [[PC.deleteOrCreate]]
 */
object PC {

  /**
   * Initialises a new presentation compiler state.
   *
   * @param workspaceURI The [[URI]] of the directory where the workspace is initialised.
   * @return A new [[PCState]] instance representing the compiler state with no compilation.
   */
  def initialise(workspaceURI: URI): PCState =
    PCState(
      workspace = Workspace.create(workspaceURI),
      buildErrors = None
    )

  /**
   * Processes a change in source or build file for a workspace that may or may not be source-aware ([[WorkspaceState.IsSourceAware]]).
   *
   * @param fileURI The URI of the changed file.
   * @param code    An optional string containing the code of the changed file.
   *                If this is [[None]], the file's content will fetched from disk.
   * @param pcState The current state of the presentation compiler.
   * @return Either an [[ErrorUnknownFileType]] if the file type is unknown,
   *         or an optional new [[PCState]] instance representing the updated compiler state.
   */
  def changed(
      fileURI: URI,
      code: Option[String],
      pcState: PCState
    )(implicit file: FileAccess,
      compiler: CompilerAccess,
      logger: ClientLogger): Either[ErrorUnknownFileType, Option[PCState]] =
    Workspace.build(
      code = Some(WorkspaceFile(fileURI, code)),
      workspace = pcState.workspace
    ) match {
      case error @ Left(_) =>
        val newPCState =
          buildChanged(
            buildChangeResult = error,
            pcState = pcState
          )

        Right(Some(newPCState))

      case Right(aware) =>
        val fileExtension =
          URIUtil.getFileExtension(fileURI)

        if (fileExtension == Build.BUILD_FILE_EXTENSION) {
          // process build change
          val buildResult =
            Workspace.buildChanged(
              buildURI = fileURI,
              code = code,
              workspace = aware
            )

          val newPCState =
            buildResult map {
              buildResult =>
                buildChanged(
                  buildChangeResult = buildResult,
                  pcState = pcState
                )
            }

          Right(newPCState)

        } else if (fileExtension == CompilerAccess.RALPH_FILE_EXTENSION) {
          // process source code change
          val sourceResult =
            Workspace.sourceCodeChanged(
              fileURI = fileURI,
              updatedCode = code,
              workspace = aware
            )

          val newPCState =
            sourceCodeChanged(
              sourceChangeResult = sourceResult,
              pcState = pcState
            )

          Right(Some(newPCState))
        } else {
          Left(ErrorUnknownFileType(fileURI))
        }
    }

  /**
   * Deletes or creates a file within the workspace, which may or may not be source-aware ([[WorkspaceState.IsSourceAware]]).
   *
   * @param events  Events to process.
   * @param pcState Current presentation compiler state.
   * @return The updated presentation compiler state containing the compilation result.
   */
  def deleteOrCreate(
      events: ArraySeq[WorkspaceFileEvent],
      pcState: PCState
    )(implicit file: FileAccess,
      compiler: CompilerAccess,
      logger: ClientLogger): PCState =
    Workspace.build(
      code = None,
      workspace = pcState.workspace
    ) match {
      case Left(error) =>
        buildChanged(
          buildChangeResult = Left(error),
          pcState = pcState
        )

      case Right(workspace) =>
        // collect events that belong to this workspace
        val workspaceEvents =
          events filter {
            event =>
              URIUtil.contains(workspace.workspaceURI, event.uri)
          }

        // is the build deleted?
        val isBuildDeleted =
          workspaceEvents.collectFirst {
            case event @ WorkspaceFileEvent.Deleted(deletedURI) if URIUtil.contains(deletedURI, workspace.buildURI) =>
              event
          }.isDefined

        val buildCode =
          if (isBuildDeleted) // if build is deleted, compile from disk
            None
          else // build exists, process from memory
            pcState.buildErrors match {
              case Some(errored) =>
                // use the code from the previous build's compilation run
                errored.codeOption

              case None =>
                // previous build was good, use the compiled build code
                Some(workspace.build.code)
            }

        // apply events to the workspace
        val newSourceCode =
          Workspace.applyEvents(
            events = workspaceEvents,
            workspace = workspace
          )

        val result =
          Workspace.compile(
            buildCode = buildCode,
            sourceCode = newSourceCode,
            currentBuild = workspace.build
          )

        buildChanged(
          buildChangeResult = result,
          pcState = pcState
        )
    }

  /**
   * Applies a build change to the [[PCState]].
   *
   * @param buildChangeResult The result of the build change operation.
   * @param pcState           The current presentation compiler state.
   * @return The updated presentation compiler state.
   */
  private def buildChanged(
      buildChangeResult: Either[BuildState.Errored, WorkspaceState],
      pcState: PCState): PCState =
    buildChangeResult match {
      case Left(buildError) =>
        // fetch the activateWorkspace to replace existing workspace
        // or-else continue with existing workspace
        val newWorkspace =
          buildError.activateWorkspace getOrElse pcState.workspace

        pcState.copy(
          buildErrors = Some(buildError),
          workspace = newWorkspace
        )

      case Right(newWorkspace) =>
        // build errors got resolved, clear it from state.
        pcState.copy(
          buildErrors = None,
          workspace = newWorkspace
        )
    }

  /**
   * Applies a source code change to the [[PCState]].
   *
   * @param sourceChangeResult The result of the source code change operation.
   * @param pcState            The current presentation compiler state.
   * @return The updated presentation compiler state.
   */
  private def sourceCodeChanged(
      sourceChangeResult: Either[BuildState.Errored, WorkspaceState],
      pcState: PCState): PCState =
    sourceChangeResult match {
      case Left(buildError) =>
        pcState.copy(buildErrors = Some(buildError))

      case Right(newWorkspace) =>
        pcState.copy(workspace = newWorkspace)
    }

}
