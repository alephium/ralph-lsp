// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc

import org.alephium.ralph.lsp.pc.workspace.WorkspaceState

import java.net.URI
import scala.collection.immutable.ArraySeq

object TestPCStates {

  def genPCStates(uris: Iterable[URI]): PCStates = {
    val workspaces =
      uris map WorkspaceState.Created

    genPCStates(workspaces.toSeq: _*)
  }

  def genPCStates(workspaces: WorkspaceState*): PCStates = {
    val pcStates =
      workspaces map {
        workspace =>
          PCState(
            workspace = workspace,
            buildErrors = None,
            tsErrors = None
          )
      }

    PCStates(pcStates.to(ArraySeq))
  }

}
