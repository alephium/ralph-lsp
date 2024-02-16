package org.alephium.ralph.lsp.pc.sourcecode

import org.alephium.ralph.lsp.access.compiler.RalphParserExtension
import org.alephium.ralph.lsp.access.compiler.ast.Tree
import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage
import org.alephium.ralph.lsp.access.compiler.message.warning.StringWarning
import org.alephium.ralph.lsp.pc.util.{StringUtil, URIUtil}
import org.alephium.ralph.{CompiledContract, CompiledScript}

import java.net.URI

sealed trait SourceCodeState {
  def fileURI: URI

  /** @see [[URIUtil.importIdentifier]] */
  def importIdentifier: Option[Tree.Import] =
    URIUtil
      .importIdentifier(fileURI)
      .flatMap(RalphParserExtension.lazyParseImportIdentifier)
}

object SourceCodeState {

  implicit val ordering: Ordering[SourceCodeState] =
    Ordering.by[SourceCodeState, URI](_.fileURI)

  /** Represents: Source code exists, but is neither Compiled or Accessed */
  sealed trait IsInitialised extends SourceCodeState

  /** Represents: Code was accessed. It can either be in Error state or Success state.
   *
   * [[OnDisk]] state is no longer achievable from this state unless the file gets removed/dropped entirely
   * from a configured workspace - [[WorkspaceState.CodeAware]].
   * */
  sealed trait IsAccessed extends SourceCodeState

  /** Represents: State where the source code is known */
  sealed trait IsCodeAware extends SourceCodeState {
    def code: String

    /** Lazily executed. Can have concurrent access. Used by code completion. */
    lazy val codeLines: Array[String] =
      StringUtil.codeLines(code)
  }

  /** Represents: Code is parsed */
  sealed trait IsParsed extends IsCodeAware

  /** Represents: Code errored */
  sealed trait IsError extends SourceCodeState

  sealed trait IsCompiled extends IsParsed

  /** The code is on disk */
  case class OnDisk(fileURI: URI) extends IsInitialised

  /** The code is in memory but not parsed or compiled */
  case class UnCompiled(fileURI: URI,
                        code: String) extends IsInitialised with IsAccessed with IsCodeAware

  /** Represents: Was unable to access code */
  case class ErrorAccess(fileURI: URI,
                         error: CompilerMessage.AnyError) extends IsError with IsAccessed

  /** Represents: Code is successfully parsed */
  case class Parsed(fileURI: URI,
                    code: String,
                    ast: Tree.Root) extends IsParsed

  /** Represents: Successful code compilation */
  case class Compiled(fileURI: URI,
                      code: String,
                      compiledCode: Seq[Either[CompiledContract, CompiledScript]],
                      parsed: SourceCodeState.Parsed) extends IsCompiled {
    def warnings: Seq[StringWarning] =
      compiledCode flatMap {
        case Left(value) =>
          value.warnings map (StringWarning(_))

        case Right(value) =>
          value.warnings map (StringWarning(_))
      }
  }

  /** Represents: Failed but it stores previous successful parse so code-completion can use this state */
  case class ErrorSource(fileURI: URI,
                         code: String,
                         errors: Seq[CompilerMessage.AnyError],
                         previous: Option[SourceCodeState.Parsed]) extends IsError with IsCompiled

}
