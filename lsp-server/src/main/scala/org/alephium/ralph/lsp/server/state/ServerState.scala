// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.server.state

import org.alephium.ralph.lsp.pc.{PCState, PCStates}
import org.alephium.ralph.lsp.server.RalphLangClient

import java.net.URI
import java.util.concurrent.{Future => JFuture}
import scala.collection.immutable.ArraySeq

/**
 * Stores Ralph's LSP server state.
 *
 * @param client   Client proxy.
 * @param listener Request listener which is canceled on server termination.
 * @param pcStates A collection of all [[PCState]].
 *                 Multiple states are created when more than one root-workspace is available.
 * @param trace    Client configured setting. See also [[org.eclipse.lsp4j.TraceValue]].
 */
case class ServerState(
    client: Option[RalphLangClient],
    listener: Option[JFuture[Void]],
    pcStates: PCStates,
    clientAllowsWatchedFilesDynamicRegistration: Boolean,
    trace: Trace,
    shutdownReceived: Boolean) {

  /**
   * Replaces an existing [[PCState]] with a matching workspace URI if it exists,
   * otherwise, inserts the new [[PCState]].
   */
  def put(newState: PCState): ServerState =
    this.copy(pcStates = pcStates put newState)

  /**
   * Removes all existing [[PCState]]s that matches the given `workspaceURI`.
   *
   * @return [[None]] if no [[PCState]]s are removed.
   *         Otherwise, returns a tuple where:
   *         - The first element is the updated [[ServerState]] after removal.
   *         - The second element is the sequence of removed [[PCState]]s.
   */
  def remove(workspaceURI: URI): Option[(ServerState, ArraySeq[PCState])] =
    pcStates.remove(workspaceURI) map {
      case (server, states) =>
        (this.copy(pcStates = server), states)
    }

}
