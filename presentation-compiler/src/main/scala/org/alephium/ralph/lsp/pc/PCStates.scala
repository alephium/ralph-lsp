// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc

import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.pc.PCStates._
import org.alephium.ralph.lsp.pc.error.ErrorSourceNotInWorkspace
import org.alephium.ralph.lsp.pc.workspace.WorkspaceState
import org.alephium.ralph.lsp.utils.CollectionUtil.CollectionUtilImplicits
import org.alephium.ralph.lsp.utils.URIUtil._

import java.net.URI
import scala.collection.immutable.ArraySeq

object PCStates {

  implicit val pcStateOrder: Ordering[PCState] =
    PCState.orderByWorkspaceId

  def empty: PCStates =
    PCStates(ArraySeq.empty)

}

/**
 * Holds a collection of [[PCState]]s, used when working with multi-root directories.
 *
 * A multi-root setup happens when the LSP's `workspaceFolder` API is used.
 *
 * @param states The state of each root directory.
 */
case class PCStates(states: ArraySeq[PCState]) extends AnyVal {

  /**
   * Replaces an existing [[PCState]] with a matching workspace URI if it exists,
   * otherwise, inserts the new [[PCState]].
   */
  def put(newState: PCState): PCStates =
    PCStates(states = states put newState)

  /**
   * Removes all existing [[PCState]]s that matches the given `workspaceURI`.
   *
   * @return [[None]] if no [[PCState]]s are removed.
   *         Otherwise, returns a tuple where:
   *         - The first element is the updated [[PCStates]] after removal.
   *         - The second element is the sequence of removed [[PCState]]s.
   */
  def remove(workspaceURI: URI): Option[(PCStates, ArraySeq[PCState])] = {
    val removedStates =
      states.filter(_.workspace.workspaceURI == workspaceURI)

    if (removedStates.isEmpty) {
      None
    } else {
      val newStates      = states.diff(removedStates)
      val newServerState = PCStates(states = newStates)
      Some((newServerState, removedStates))
    }
  }

  /**
   * Gets the [[PCState]] of a workspace that contains the given `fileURI`.
   */
  def findContains(fileURI: URI): Option[PCState] =
    states find {
      state =>
        state.workspace match {
          case workspace: WorkspaceState.IsSourceAware if CompilerAccess.isRalphFileExtension(fileURI) =>
            /*
             * When the workspace is source-aware and the file is a `.ral` source-file, the file must processed
             * be within the configured `contractURI` or the configured `dependencyPath`. Any other file must not be processed.
             *
             * The check must on `contractURI` and `dependencyPath`, and not an upper level check on `state.workspace.workspaceURI`
             * because if there is a "Workspace-A" containing a `dependencies` folders that it itself does not point to,
             * but another "Workspace-B" uses "Workspace-A"'s `dependencies` path as its dependency, then simply
             * invoking `state.workspace.workspaceURI contains fileURI` will return "Workspace-A"'s `PCState`
             * which would be incorrect.
             *
             * TODO: Implement the above case as a test-case.
             */
            workspace.build.contractURI.contains(fileURI) || workspace.build.dependencyPath.contains(fileURI)

          case _ =>
            // If for some reason the workspace is not compiled, then the best we can do is search for it at the `workspaceURI` level.
            state.workspace.workspaceURI contains fileURI
        }
    }

  /**
   * Gets the [[PCState]] for the workspace containing `fileURI`.
   * Returns an error if the file isn't part of any known workspace.
   *
   * @param fileURI The `URI` of a file inside a workspace.
   * @return Either the error [[ErrorSourceNotInWorkspace]] or the matching [[PCState]].
   */
  def get(fileURI: URI): Either[ErrorSourceNotInWorkspace, PCState] =
    findContains(fileURI) match {
      case Some(found) =>
        Right(found)

      case None =>
        Left(ErrorSourceNotInWorkspace(fileURI))
    }

}
