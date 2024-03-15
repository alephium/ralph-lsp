package org.alephium.ralph.lsp.pc.workspace

import java.net.URI

/**
 * Events dispatched by LSP-client for files & folders that are being watched.
 */
sealed trait WorkspaceFileEvent {
  def uri: URI
}

object WorkspaceFileEvent {

  /** A file or folder is created */
  case class Created(uri: URI) extends WorkspaceFileEvent

  /** A file or folder is deleted */
  case class Deleted(uri: URI) extends WorkspaceFileEvent
}
