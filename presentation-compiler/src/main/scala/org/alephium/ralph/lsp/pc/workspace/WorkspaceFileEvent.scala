// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.workspace

import java.net.URI

/**
 * <a href="https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#fileChangeType">File Change Events</a>
 * dispatched by LSP-client for files & folders that are being watched.
 */
sealed trait WorkspaceFileEvent {

  def uri: URI

}

object WorkspaceFileEvent {

  /** Represents the creation of a file or folder */
  case class Created(uri: URI) extends WorkspaceFileEvent

  /** Represents the deletion of a file or folder */
  case class Deleted(uri: URI) extends WorkspaceFileEvent

  /** Represents a change to a file */
  case class Changed(uri: URI) extends WorkspaceFileEvent

}
