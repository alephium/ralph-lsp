// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc

import org.alephium.ralph.lsp.pc.workspace.WorkspaceState

import java.net.URI
import scala.collection.immutable.ArraySeq

object TestMultiPCState {

  def genMultiPCState(uris: Iterable[URI]): MultiPCState = {
    val pcStates =
      uris map {
        fileURI =>
          PCState(
            workspace = WorkspaceState.Created(fileURI),
            buildErrors = None,
            tsErrors = None
          )
      }

    MultiPCState(pcStates.to(ArraySeq))
  }

}
