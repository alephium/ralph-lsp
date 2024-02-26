package org.alephium.ralph.lsp.pc.workspace.build.error

import org.alephium.ralph.lsp.access.compiler.message.{CompilerMessage, SourceIndex}

case class ErrorDependencyPathIsWithinContractPath(index: SourceIndex) extends CompilerMessage.Error {
  override def message: String =
    "`dependencyPath` and `contractPath` cannot be identical to or nested within each other. Make sure that they are distinct and do not overlap."
}
