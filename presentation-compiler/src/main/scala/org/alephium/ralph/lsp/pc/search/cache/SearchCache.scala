// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.cache

import org.alephium.ralph.lsp.pc.workspace.WorkspaceState
import org.alephium.ralph.lsp.utils.log.ClientLogger
import org.alephium.ralph.lsp.utils.WeakHashMapBase

import java.util

case class SearchCache() extends WeakHashMapBase[WorkspaceState.IsSourceAware, WorkspaceSearchCache](new util.WeakHashMap(1, 1)) {

  def get(
      workspace: WorkspaceState.IsSourceAware
    )(implicit logger: ClientLogger): WorkspaceSearchCache =
    getOrPut(
      key = workspace,
      value = new WorkspaceSearchCache(workspace)
    )

}
