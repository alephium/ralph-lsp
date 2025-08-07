// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.cache

import org.alephium.ralph.lsp.pc.workspace.WorkspaceState
import org.alephium.ralph.lsp.utils.log.ClientLogger
import org.alephium.ralph.lsp.utils.WeakHashMapBase

import java.util

/**
 * A cache that stores [[WorkspaceState]] related search information.
 *
 * The [[util.WeakHashMap]] is configured to `initialCapacity` and `loadFactor` both set to `1` because
 * workspace are not expected to change often and dynamically.
 *
 * `loadFactor = 1` because the workspaces are expected to be mostly single-root workspaces, rather than multi-root workspaces.
 *
 * TODO: Check if setting [[maxWorkspaces]] to the currently configured multi-root workspaces improves performance.
 */
case class SearchCache(maxWorkspaces: Int) extends WeakHashMapBase[WorkspaceState.IsSourceAware, WorkspaceSearchCache](new util.WeakHashMap(maxWorkspaces, 1)) {

  def get(
      workspace: WorkspaceState.IsSourceAware
    )(implicit logger: ClientLogger): WorkspaceSearchCache =
    getOrPut(
      key = workspace,
      value = new WorkspaceSearchCache(workspace)
    )

}
