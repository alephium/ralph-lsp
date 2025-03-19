// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.server

import org.alephium.ralph.lsp.pc.{MultiPCState, PCState}
import org.alephium.ralph.lsp.pc.workspace.WorkspaceState
import org.alephium.ralph.lsp.server.state.{ServerState, Trace}

import java.net.URI
import scala.collection.immutable.ArraySeq
import scala.util.Random

object TestServerState {

  def genServerState(uris: Iterable[URI]): ServerState = {
    val pcStates =
      uris map {
        fileURI =>
          PCState(
            workspace = WorkspaceState.Created(fileURI),
            buildErrors = None,
            tsErrors = None
          )
      }

    ServerState(
      client = None,
      listener = None,
      multiPCState = MultiPCState(pcStates.to(ArraySeq)),
      clientAllowsWatchedFilesDynamicRegistration = Random.nextBoolean(),
      trace = Random.shuffle(Trace.all.toList).head,
      shutdownReceived = Random.nextBoolean()
    )
  }

}
