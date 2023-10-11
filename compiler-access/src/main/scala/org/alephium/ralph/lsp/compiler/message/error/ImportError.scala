package org.alephium.ralph.lsp.compiler.message.error

import org.alephium.ralph.lsp.compiler.message.{CompilerMessage, SourceIndex}

sealed trait ImportError extends CompilerMessage.Error

object ImportError {
  final case class Unknown(importValue: String, index: SourceIndex) extends ImportError {
    val message: String = s"Unknown import: $importValue"
  }
}
