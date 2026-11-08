// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.cache

import org.alephium.ralph.lsp.pc.workspace.WorkspaceState
import org.alephium.ralph.lsp.utils.log.ClientLogger
import org.alephium.ralph.lsp.utils.WeakHashMapBase

import java.net.URI
import java.util

/**
 * Weak snapshot lookup plus a bounded set of recent roots for incremental reuse.
 * Keeping the latest snapshot alive lets unchanged file extraction survive GC between edits,
 * without keeping previous workspace versions alive. Eviction affects performance only.
 */
case class SearchCache(maxWorkspaces: Int) extends WeakHashMapBase[WorkspaceState.IsSourceAware, WorkspaceSearchCache](new util.WeakHashMap(maxWorkspaces, 1)) {

  private val retainedRoots = math.max(1, maxWorkspaces)
  private val latest        = new util.LinkedHashMap[URI, WorkspaceSearchCache](retainedRoots, 1.0f, true)

  def get(
      workspace: WorkspaceState.IsSourceAware
    )(implicit logger: ClientLogger): WorkspaceSearchCache =
    synchronized {
      val result = getOrPut(
        key = workspace,
        value = new WorkspaceSearchCache(workspace, Option(latest.get(workspace.workspaceURI)))
      )
      latest.put(workspace.workspaceURI, result)
      if (latest.size() > retainedRoots) {
        val iterator = latest.keySet().iterator()
        iterator.next()
        iterator.remove()
      }
      result
    }

  private[cache] def retainedWorkspaceCount: Int =
    synchronized(latest.size())

  /** Release strong retention when a workspace closes; snapshot lookups remain weak. */
  def release(workspaceURI: URI): Unit =
    synchronized {
      latest.remove(workspaceURI)
      ()
    }

  /** Release strong retention when the server shuts down. */
  def clearRetained(): Unit =
    synchronized {
      latest.clear()
    }

}
