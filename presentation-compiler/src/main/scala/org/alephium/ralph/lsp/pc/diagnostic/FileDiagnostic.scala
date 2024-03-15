package org.alephium.ralph.lsp.pc.diagnostic

import java.net.URI

case class FileDiagnostic(fileURI: URI, codeDiagnostics: Seq[CodeDiagnostic])
