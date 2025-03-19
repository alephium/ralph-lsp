// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc

import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage
import org.alephium.ralph.lsp.pc.notification.{ErrorSourceNotInWorkspace, ErrorWorkspaceFolderNotSupplied}
import org.alephium.ralph.lsp.pc.MultiPCState._
import org.alephium.ralph.lsp.utils.CollectionUtil.CollectionUtilImplicits
import org.alephium.ralph.lsp.utils.URIUtil.URIUtilImplicits

import java.net.URI
import scala.collection.immutable.ArraySeq

object MultiPCState {

  implicit val pcStateOrder: Ordering[PCState] =
    PCState.orderByWorkspaceId

  def empty: MultiPCState =
    MultiPCState(ArraySeq.empty)

}

/**
 * Holds a collection of [[PCState]]s, used when working with multi-root directories.
 *
 * A multi-root setup happens when the LSP's `workspaceFolder` API is used.
 *
 * @param states The state of each root directory.
 */
case class MultiPCState(states: ArraySeq[PCState]) extends AnyVal {

  def isEmpty: Boolean =
    states.isEmpty

  /**
   * Replaces an existing [[PCState]] with a matching workspace URI if it exists,
   * otherwise, inserts the new [[PCState]].
   */
  def put(newState: PCState): MultiPCState =
    MultiPCState(states = states put newState)

  /**
   * Removes all existing [[PCState]]s that matches the given `workspaceURI`.
   *
   * @return [[None]] if no [[PCState]]s are removed.
   *         Otherwise, returns a tuple where:
   *         - The first element is the updated [[MultiPCState]] after removal.
   *         - The second element is the sequence of removed [[PCState]]s.
   */
  def remove(workspaceURI: URI): Option[(MultiPCState, ArraySeq[PCState])] = {
    val removedStates =
      states.filter(_.workspace.workspaceURI == workspaceURI)

    if (removedStates.isEmpty) {
      None
    } else {
      val newStates      = states.diff(removedStates)
      val newServerState = MultiPCState(states = newStates)
      Some((newServerState, removedStates))
    }
  }

  /**
   * Gets the [[PCState]] of a workspace that contains the given `fileURI`.
   */
  def findContains(fileURI: URI): Option[PCState] =
    states.find(_.workspace.workspaceURI contains fileURI)

  /**
   * Gets the [[PCState]] for the workspace containing `fileURI`, if it exists.
   * If no matching workspace is found, it returns all [[PCState]]s.
   *
   * @param fileURI The `URI` of a file inside a workspace.
   * @return Either an error or an array of [[PCState]]s.
   */
  def getOneOrAll(fileURI: URI): Either[CompilerMessage.Error, ArraySeq[PCState]] =
    get(fileURI)
      .map(ArraySeq(_))
      .orElse(getAll())

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

  /**
   * Returns all available [[PCState]]s, or an error if no workspaces exist.
   * This shouldn't normally happen since `initialized` is always called first.
   *
   * @return Either the error [[ErrorWorkspaceFolderNotSupplied]] or an array of all [[PCState]]s.
   */
  private def getAll(): Either[ErrorWorkspaceFolderNotSupplied.type, ArraySeq[PCState]] =
    if (states.isEmpty)
      Left(ErrorWorkspaceFolderNotSupplied)
    else
      Right(states)

}
