package org.alephium.ralph.lsp.server

import java.util.UUID

object MessageMethods {
  val WORKSPACE_WATCHED_FILES = "workspace/didChangeWatchedFiles"

  /* Unique ids used to register/unregister capabilities */
  val WORKSPACE_WATCHED_FILES_ID: String = UUID.randomUUID().toString()
}

