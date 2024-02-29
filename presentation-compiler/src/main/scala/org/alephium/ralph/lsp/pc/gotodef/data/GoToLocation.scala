package org.alephium.ralph.lsp.pc.gotodef.data

import org.alephium.ralph.lsp.access.compiler.message.CodeRange

import java.net.URI

case class GoToLocation(uri: URI,
                        codeRange: CodeRange)
