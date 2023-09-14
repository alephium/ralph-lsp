package org.alephium.ralph.lsp.pc.sourcecode

import org.alephium.ralph.{CompiledContract, CompiledScript}
import org.alephium.ralph.error.CompilerError.FormattableError
import org.alephium.ralph.Ast.ContractWithState

import java.net.URI

sealed trait SourceCodeState {
  def fileURI: URI
}

object SourceCodeState {

  /** Represents: Code was accessed. It can either be in Error state or Success state.
   *
   * [[OnDisk]] state is no longer achievable from this state unless the file gets removed/dropped entirely
   * from a configured workspace - [[org.alephium.ralph.lsp.pc.workspace.WorkspaceState.Configured]].
   * */
  sealed trait AccessedState extends SourceCodeState

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
  case class OnDisk(fileURI: URI) extends SourceCodeState

  /** The code is in memory but not parsed or compiled */
  case class UnCompiled(fileURI: URI,
                        code: String) extends AccessedState with CachedState

  /** Represents: Was unable to access code */
  case class ErrorAccess(fileURI: URI,
                         error: FormattableError) extends ErrorState with AccessedState {
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
