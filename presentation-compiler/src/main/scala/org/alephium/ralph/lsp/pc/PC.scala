package org.alephium.ralph.lsp.pc

import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.log.ClientLogger
import org.alephium.ralph.lsp.pc.util.URIUtil
import org.alephium.ralph.lsp.pc.workspace.build.error.ErrorUnknownFileType
import org.alephium.ralph.lsp.pc.workspace.{Workspace, WorkspaceFile, WorkspaceFileEvent, WorkspaceState}
import org.alephium.ralph.lsp.pc.workspace.build.{Build, BuildState}

import java.net.URI
import scala.collection.immutable.ArraySeq

object PC {

  /**
   * Initialises a new presentation compiler.
   *
   * @param workspaceURI The [[URI]] of the directory where the workspace is initialised.
   * @return A new [[PCState]] instance representing the compiler state with no compilation.
   */
  def initialise(workspaceURI: URI): PCState =
    PCState(
      workspace = Workspace.create(workspaceURI),
      buildErrors = None
    )

  /** Process source or build file change for a workspace that may or may not be source-aware ([[WorkspaceState.IsSourceAware]]) */
  def changed(fileURI: URI,
              code: Option[String],
              pcState: PCState)(implicit file: FileAccess,
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
   * Delete or create a file within the workspace that may or may not be source-aware ([[WorkspaceState.IsSourceAware]])
   *
   * @param events  Events to process
   * @param pcState Current workspace and build state
   * @return Workspace change result
   * */
  def deleteOrCreate(events: ArraySeq[WorkspaceFileEvent],
                     pcState: PCState)(implicit file: FileAccess,
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
          workspaceEvents contains WorkspaceFileEvent.Deleted(workspace.buildURI)

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

  /** Apply build change to the [[PCState]] */
  private def buildChanged(buildChangeResult: Either[BuildState.BuildErrored, WorkspaceState],
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

  /** Apply source-code change to the [[PCState]] */
  private def sourceCodeChanged(sourceChangeResult: Either[BuildState.BuildErrored, WorkspaceState],
                                pcState: PCState): PCState =
    sourceChangeResult match {
      case Left(buildError) =>
        pcState.copy(buildErrors = Some(buildError))

      case Right(newWorkspace) =>
        pcState.copy(workspace = newWorkspace)
    }
}
