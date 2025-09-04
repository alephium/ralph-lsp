// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc

import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.workspace.{Workspace, WorkspaceFile, WorkspaceFileEvent, WorkspaceState}
import org.alephium.ralph.lsp.pc.workspace.build.{Build, BuildError}
import org.alephium.ralph.lsp.pc.workspace.build.error.ErrorUnknownFileType
import org.alephium.ralph.lsp.pc.workspace.build.typescript.TSBuild
import org.alephium.ralph.lsp.utils.log.{ClientLogger, StrictImplicitLogging}
import org.alephium.ralph.lsp.utils.URIUtil

import java.net.URI
import scala.collection.immutable.ArraySeq

/**
 * Implements public presentation compiler APIs.
 *
 * To invoke a compilation, there are only two ways:
 *  - When a file is changed, use [[PC.changed]]
 *  - When files are deleted, created or when changes span multiple files, use [[PC.events]]
 */
object PC extends StrictImplicitLogging {

  /**
   * Initialises a new presentation compiler state.
   *
   * @param workspaceURI The [[URI]] of the directory where the workspace is initialised.
   * @return A new [[PCState]] instance representing the compiler state with no compilation.
   */
  def initialise(workspaceURI: URI): PCState =
    PCState(
      workspace = Workspace.create(workspaceURI),
      buildErrors = None,
      tsErrors = None
    )

  /**
   * Processes a change in source or build file for a workspace that may or may not be source-aware ([[WorkspaceState.IsSourceAware]]).
   *
   * @param fileURI The URI of the changed file.
   * @param code    An optional string containing the code of the changed file.
   *                If this is [[None]], the file's content will be fetched from disk.
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
            changedFileURI = Some(fileURI),
            buildChangeResult = error,
            pcState = pcState
          )

        Right(Some(newPCState))

      case Right(aware) =>
        val fileExtension =
          URIUtil.getFileExtension(fileURI)

        if (fileExtension == Build.FILE_EXTENSION || fileExtension == TSBuild.FILE_EXTENSION) {
          // process build change
          val buildResult =
            Workspace.buildChanged(
              buildURI = fileURI,
              code = code,
              workspace = aware,
              buildErrors = pcState.buildErrors
            )

          val newPCState =
            buildResult map {
              buildResult =>
                buildChanged(
                  changedFileURI = Some(fileURI),
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
   * Processes deletion, creation or modifications to multiple files.
   *
   * @param events  Events to process.
   * @param pcState Current presentation compiler state.
   * @return The updated presentation compiler state containing the compilation result.
   */
  def events(
      events: ArraySeq[WorkspaceFileEvent],
      pcState: PCState
    )(implicit file: FileAccess,
      compiler: CompilerAccess,
      logger: ClientLogger): PCState = {
    val nextPCState =
      jsonBuildEvents(
        events = events,
        pcState = pcState
      )

    tsBuildEvents(
      events = events,
      pcState = nextPCState
    ) getOrElse nextPCState
  }

  /**
   * Manages the deletion, creation and changes to all files managed by the `ralph.json` build file.
   * These files include all `.ral` source files.
   *
   * For processing the `alephium.config.ts` build file, see [[tsBuildEvents]].
   *
   * @param events  Events to process.
   * @param pcState Current presentation compiler state.
   * @return The updated presentation compiler state containing the compilation result.
   */
  private def jsonBuildEvents(
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
          changedFileURI = None,
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
          Workspace
            .compile(
              buildCode = buildCode,
              sourceCode = newSourceCode,
              currentBuild = workspace.build
            )
            .left
            .map(BuildError(_))

        buildChanged(
          changedFileURI = None,
          buildChangeResult = result,
          pcState = pcState
        )
    }

  /**
   * Manages the deletion or creation of the `alephium.config.ts` file.
   * This build file does not manage `.ral` source files.
   * It only mutates the `ralph.json` build file.
   *
   * For processing `ralph.json` and its dependent `.ral` source files, see [[jsonBuildEvents]].
   *
   * @param events  Events to process.
   * @param pcState Current presentation compiler state.
   * @return The updated presentation compiler state containing the compilation result.
   */
  private def tsBuildEvents(
      events: ArraySeq[WorkspaceFileEvent],
      pcState: PCState
    )(implicit file: FileAccess,
      compiler: CompilerAccess,
      logger: ClientLogger): Option[PCState] =
    // Check: Did an event occur for the `alephium.config.ts` file?
    if (events exists (_.uri == pcState.workspace.tsBuildURI))
      // Execute `changed` so a rebuild occurs.
      changed(
        fileURI = pcState.workspace.tsBuildURI,
        code = None, // events do not contain code, fetch from disk.
        pcState = pcState
      ) match {
        case Left(error: ErrorUnknownFileType) =>
          // This should never occur since `tsBuildURI` is a known file type.
          logger.error(error.message)
          None

        case Right(nextPCState) =>
          nextPCState
      }
    else
      None

  /**
   * Applies a build change to the [[PCState]].
   *
   * @param buildChangeResult The result of the build change operation.
   * @param pcState           The current presentation compiler state.
   * @return The updated presentation compiler state.
   */
  private def buildChanged(
      changedFileURI: Option[URI],
      buildChangeResult: Either[BuildError, WorkspaceState],
      pcState: PCState): PCState = {
    def tsErrors() =
      if (changedFileURI contains pcState.workspace.tsBuildURI) // Check: Was this function called due to `alephium.config.ts` changing?
        // `alephium.config.ts` errors are resolved only if the changed file was the `alephium.config.ts` build file.
        None
      else
        // Else continue with existing `alephium.config.ts` errors as they were not resolved.
        pcState.tsErrors

    buildChangeResult.left.map(_.error) match {
      case Left(Right(buildError)) =>
        // fetch the activateWorkspace to replace existing workspace
        // or-else continue with existing workspace
        val newWorkspace =
          buildError.activateWorkspace getOrElse pcState.workspace

        PCState(
          workspace = newWorkspace,
          buildErrors = Some(buildError),
          tsErrors = tsErrors()
        )

      case Left(Left(tsErrors)) =>
        PCState(
          workspace = pcState.workspace,
          buildErrors = pcState.buildErrors,
          tsErrors = Some(tsErrors)
        )

      case Right(newWorkspace) =>
        PCState(
          workspace = newWorkspace,
          // A new workspace was returned! This can only happen if `ralph.json` errors were resolved,
          // so clear those errors, if any.
          buildErrors = None,
          tsErrors = tsErrors()
        )
    }
  }

  /**
   * Applies a source code change to the [[PCState]].
   *
   * @param sourceChangeResult The result of the source code change operation.
   * @param pcState            The current presentation compiler state.
   * @return The updated presentation compiler state.
   */
  private def sourceCodeChanged(
      sourceChangeResult: Either[BuildError, WorkspaceState],
      pcState: PCState): PCState =
    sourceChangeResult.left.map(_.error) match {
      case Left(Right(jsonBuildError)) =>
        pcState.copy(buildErrors = Some(jsonBuildError))

      case Left(Left(tsBuildError)) =>
        pcState.copy(tsErrors = Some(tsBuildError))

      case Right(newWorkspace) =>
        pcState.copy(workspace = newWorkspace)
    }

}
