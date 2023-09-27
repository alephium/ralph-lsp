package org.alephium.ralph.lsp.pc.sourcecode

import org.alephium.ralph.{CompiledContract, CompiledScript}
import org.alephium.ralph.error.CompilerError.FormattableError
import org.alephium.ralph.Ast.ContractWithState
import org.alephium.ralph.lsp.compiler.error.StringWarning

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
   * from a configured workspace - [[WorkspaceState.BuildAware]].
   * */
  sealed trait AccessedState extends SourceCodeState

  /** Represents: State where the source code is known */
  sealed trait CodeAware extends SourceCodeState {
    def code: String
  }

  /** Represents: Code is parsed */
  sealed trait ParsedState extends CodeAware

  /** Represents: Code errored */
  sealed trait FailedState extends SourceCodeState

  /** The code is on disk */
  case class OnDisk(fileURI: URI) extends SourceCodeState

  /** The code is in memory but not parsed or compiled */
  case class UnCompiled(fileURI: URI,
                        code: String) extends AccessedState with CodeAware

  /** Represents: Was unable to access code */
  case class ErrorAccess(fileURI: URI,
                         error: FormattableError) extends FailedState with AccessedState

  /** Represents: Code is successfully parsed */
  case class Parsed(fileURI: URI,
                    code: String,
                    contracts: Seq[ContractWithState]) extends ParsedState

  /** Represents: Successful code compilation */
  case class Compiled(fileURI: URI,
                      code: String,
                      compiledCode: Seq[Either[CompiledContract, CompiledScript]],
                      parsed: SourceCodeState.Parsed) extends ParsedState {
    def warnings: Seq[StringWarning] =
      compiledCode.flatMap {
        case Left(value) =>
          value.warnings map StringWarning

        case Right(value) =>
          value.warnings map StringWarning
      }
  }

  /** Represents: Failed but it stores previous successful parse so code-completion can use this state */
  case class ErrorSource(fileURI: URI,
                         code: String,
                         errors: Seq[FormattableError],
                         previous: Option[SourceCodeState.Parsed]) extends FailedState with CodeAware

}
