package org.alephium.ralph.lsp.pc.workspace

import java.net.URI

case class WorkspaceFile(fileURI: URI,
                         text: Option[String])
