package org.alephium.ralph.lsp.pc.workspace

import java.net.URI

/**
 * Events dispatched by LSP-client for files & folders that are being watched.
 */
sealed trait WorkspaceFileEvent {
  def fileURI: URI
}

object WorkspaceFileEvent {
  /** A file is created */
  case class Created(fileURI: URI) extends WorkspaceFileEvent

  /** A file got changed */
  case class Changed(fileURI: URI) extends WorkspaceFileEvent

  /** A file got deleted */
  case class Deleted(fileURI: URI) extends WorkspaceFileEvent
}
