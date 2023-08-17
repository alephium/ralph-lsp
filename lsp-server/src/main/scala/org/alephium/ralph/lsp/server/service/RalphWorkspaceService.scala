package org.alephium.ralph.lsp.server.service

import org.eclipse.lsp4j.services.WorkspaceService
import org.eclipse.lsp4j.{DidChangeConfigurationParams, DidChangeWatchedFilesParams}

object RalphWorkspaceService extends WorkspaceService {
  override def didChangeConfiguration(params: DidChangeConfigurationParams): Unit =
    ()

  override def didChangeWatchedFiles(params: DidChangeWatchedFilesParams): Unit =
    ()
}
