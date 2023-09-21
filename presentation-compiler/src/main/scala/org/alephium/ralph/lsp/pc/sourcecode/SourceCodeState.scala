package org.alephium.ralph.lsp.pc.sourcecode

import org.alephium.ralph.{CompiledContract, CompiledScript}
import org.alephium.ralph.error.CompilerError.FormattableError
import org.alephium.ralph.Ast.ContractWithState
import org.alephium.ralph.lsp.compiler.error.StringMessage

import java.net.URI

sealed trait SourceCodeState {
  def fileURI: URI
}

object SourceCodeState {

  implicit val ordering: Ordering[SourceCodeState] =
    Ordering.by[SourceCodeState, URI](_.fileURI)

  /** Represents: Code was accessed. It can either be in Error state or Success state.
   *
   * [[OnDisk]] state is no longer achievable from this state unless the file gets removed/dropped entirely
   * from a configured workspace - [[org.alephium.ralph.lsp.pc.workspace.WorkspaceState.BuildAware]].
   * */
  sealed trait AccessedState extends SourceCodeState

  /** Represents: Code cached */
  sealed trait CachedState extends SourceCodeState {
    def code: String
  }

  /** Represents: Code is parsed */
  sealed trait ParsedState extends CachedState

  /** Represents: Code errored */
  sealed trait FailedState extends SourceCodeState {
    def updateError(error: FormattableError): FailedState
  }

  /** The code is on disk */
  case class OnDisk(fileURI: URI) extends SourceCodeState

  /** The code is in memory but not parsed or compiled */
  case class UnCompiled(fileURI: URI,
                        code: String) extends AccessedState with CachedState

  /** Represents: Was unable to access code */
  case class ErrorAccess(fileURI: URI,
                         error: FormattableError) extends FailedState with AccessedState {
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
                      parsed: SourceCodeState.Parsed) extends ParsedState {
    def warnings: Seq[StringMessage] =
      compiledCode.flatMap {
        case Left(value) =>
          value.warnings map StringMessage

        case Right(value) =>
          value.warnings map StringMessage
      }
  }

  /** Represents: Failed but it stores previous successful parse so code-completion can use this state */
  case class ErrorSource(fileURI: URI,
                         code: String,
                         errors: Seq[FormattableError],
                         previous: Option[SourceCodeState.Parsed]) extends FailedState with CachedState {
    override def updateError(error: FormattableError): ErrorSource =
      this.copy(errors = Seq(error))
  }

}
