package org.alephium.ralph.lsp.pc.sourcecode

import org.alephium.ralph.{Ast, CompiledContract, CompiledScript}
import org.alephium.ralph.error.CompilerError.FormattableError

import java.net.URI

sealed trait SourceCodeState {
  def fileURI: URI
}

object SourceCodeState {

  sealed trait ReadState extends SourceCodeState
  sealed trait ParsedState extends SourceCodeState
  sealed trait FailedState extends SourceCodeState

  case class OnDisk(fileURI: URI) extends ReadState

  case class UnCompiled(fileURI: URI,
                        code: String) extends ReadState

  case class FailedAccess(fileURI: URI,
                          exception: Throwable) extends FailedState with ReadState

  case class Parsed(fileURI: URI,
                    code: String,
                    parsedAST: Ast.MultiContract) extends ParsedState

  case class Compiled(fileURI: URI,
                      code: String,
                      compiledCode: Seq[Either[CompiledContract, CompiledScript]],
                      previousState: SourceCodeState.Parsed) extends ParsedState

  case class Errored(fileURI: URI,
                     code: String,
                     errors: Seq[FormattableError],
                     previous: Option[SourceCodeState.Parsed]) extends FailedState

}
