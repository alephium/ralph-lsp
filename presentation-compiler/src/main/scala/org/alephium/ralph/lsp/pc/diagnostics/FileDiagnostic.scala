package org.alephium.ralph.lsp.pc.diagnostics

import java.net.URI

case class FileDiagnostic(fileURI: URI,
                          diagnostics: Seq[CodeDiagnostic])
