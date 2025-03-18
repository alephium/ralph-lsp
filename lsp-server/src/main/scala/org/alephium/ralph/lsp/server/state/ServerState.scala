// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.server.state

import org.alephium.ralph.lsp.pc.PCState
import org.alephium.ralph.lsp.server.RalphLangClient
import org.alephium.ralph.lsp.utils.CollectionUtil.CollectionUtilImplicits
import org.alephium.ralph.lsp.utils.URIUtil.URIUtilImplicits

import java.net.URI
import java.util.concurrent.{Future => JFuture}
import scala.collection.immutable.ArraySeq

/**
 * Stores Ralph's LSP server state.
 *
 * @param client   Client proxy.
 * @param listener Request listener which is canceled on server termination.
 * @param pcState  Presentation-compiler state
 * @param trace    Client configured setting. See also [[org.eclipse.lsp4j.TraceValue]].
 */
case class ServerState(
    client: Option[RalphLangClient],
    listener: Option[JFuture[Void]],
    pcState: ArraySeq[PCState],
    clientAllowsWatchedFilesDynamicRegistration: Boolean,
    trace: Trace,
    shutdownReceived: Boolean) {

  private implicit val order: Ordering[PCState] =
    Ordering.by(_.workspace.workspaceURI)

  /**
   * Replaces an existing [[PCState]] with a matching workspace URI if it exists,
   * otherwise, inserts the new [[PCState]].
   */
  def put(newState: PCState): ServerState =
    this.copy(pcState = pcState put newState)

  /**
   * Removes all existing [[PCState]]s that matches the given `workspaceURI`.
   *
   * @return [[None]] if no [[PCState]]s are removed.
   *         Otherwise, returns a tuple where:
   *         - The first element is the updated [[ServerState]] after removal.
   *         - The second element is the sequence of removed [[PCState]]s.
   */
  def remove(workspaceURI: URI): Option[(ServerState, ArraySeq[PCState])] = {
    val removedStates =
      pcState.filter(_.workspace.workspaceURI == workspaceURI)

    if (removedStates.isEmpty) {
      None
    } else {
      val newStates      = pcState.diff(removedStates)
      val newServerState = this.copy(pcState = newStates)
      Some((newServerState, removedStates))
    }
  }

  /**
   * Gets the [[PCState]] of a workspace that contains the given fileURI.
   */
  def findContains(fileURI: URI): Option[PCState] =
    pcState.find(_.workspace.workspaceURI contains fileURI)

}
