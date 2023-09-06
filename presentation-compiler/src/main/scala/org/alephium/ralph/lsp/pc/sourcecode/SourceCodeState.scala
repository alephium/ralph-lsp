package org.alephium.ralph.lsp.pc.sourcecode

import org.alephium.ralph.{Ast, CompiledContract, CompiledScript}
import org.alephium.ralph.error.CompilerError.FormattableError

import java.net.URI

sealed trait SourceCodeState {
  def fileURI: URI
}

object SourceCodeState {

  /** Represents: Code was access */
  sealed trait AccessState extends SourceCodeState

  /** Represents: Code is parsed */
  sealed trait ParsedState extends SourceCodeState

  /** Represents: Code errored */
  sealed trait FailedState extends SourceCodeState

  /** The code is on disk */
  case class OnDisk(fileURI: URI) extends AccessState

  /** The code is in memory but not parsed or compiled */
  case class UnCompiled(fileURI: URI,
                        code: String) extends AccessState

  /** Represents: Was unable to access code */
  case class FailedAccess(fileURI: URI,
                          exception: Throwable) extends FailedState with AccessState

  /** Represents: Code is successfully parsed */
  case class Parsed(fileURI: URI,
                    code: String,
                    parsedAST: Ast.MultiContract) extends ParsedState

  /** Represents: Successful code compilation */
  case class Compiled(fileURI: URI,
                      code: String,
                      compiledCode: Seq[Either[CompiledContract, CompiledScript]],
                      previousState: SourceCodeState.Parsed) extends ParsedState

  /** Represents: Failed but it stores previous successful parse so code-completion can use this state */
  case class Errored(fileURI: URI,
                     code: String,
                     errors: Seq[FormattableError],
                     previous: Option[SourceCodeState.Parsed]) extends FailedState

}
