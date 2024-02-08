package org.alephium.ralph.lsp.pc.completion

import org.alephium.ralph.lsp.pc.workspace.WorkspaceState

import java.net.URI

object CodeCompleter {

  /**
   * Provides code completion for the given workspace state.
   *
   * @param line      Line position in a document (zero-based).
   * @param character Character offset on a line in a document (zero-based).
   * @param uri       The text document's uri.
   * @param workspace Current workspace state.
   * @return
   */
  def complete(line: Int,
               character: Int,
               uri: URI,
               workspace: WorkspaceState.IsSourceAware): Array[Suggestion] =
    Array.empty[Suggestion] // TODO

}
