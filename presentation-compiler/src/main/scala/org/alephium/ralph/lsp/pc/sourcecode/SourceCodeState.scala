package org.alephium.ralph.lsp.pc.sourcecode

import org.alephium.ralph.{CompiledContract, CompiledScript}
import org.alephium.ralph.error.CompilerError.FormattableError
import org.alephium.ralph.Ast.ContractWithState

import java.net.URI

sealed trait SourceCodeState {
  def fileURI: URI
}

object SourceCodeState {

  /** Represents: Code was access */
  sealed trait AccessState extends SourceCodeState

  /** Represents: Code cached */
  sealed trait CachedState extends SourceCodeState {
    def code: String
  }

  /** Represents: Code is parsed */
  sealed trait ParsedState extends CachedState

  /** Represents: Code errored */
  sealed trait ErrorState extends SourceCodeState {
    def updateError(error: FormattableError): ErrorState
  }

  /** The code is on disk */
  case class OnDisk(fileURI: URI) extends AccessState

  /** The code is in memory but not parsed or compiled */
  case class UnCompiled(fileURI: URI,
                        code: String) extends AccessState with CachedState

  /** Represents: Was unable to access code */
  case class ErrorAccess(fileURI: URI,
                         error: FormattableError) extends ErrorState with AccessState {
    override def updateError(error: FormattableError): ErrorAccess =
      this.copy(error = error)
  }

  /** Represents: Code is successfully parsed */
  case class Parsed(fileURI: URI,
                    code: String,
                    contracts: Seq[ContractWithState]) extends ParsedState

  /** Represents: Successful code compilation */
  case class Compiled(fileURI: URI,
                      code: String,
                      compiledCode: Seq[Either[CompiledContract, CompiledScript]],
                      previousState: SourceCodeState.Parsed) extends ParsedState

  /** Represents: Failed but it stores previous successful parse so code-completion can use this state */
  case class Errored(fileURI: URI,
                     code: String,
                     errors: Seq[FormattableError],
                     previous: Option[SourceCodeState.Parsed]) extends ErrorState with CachedState {
    override def updateError(error: FormattableError): Errored =
      this.copy(errors = Seq(error))
  }

}
