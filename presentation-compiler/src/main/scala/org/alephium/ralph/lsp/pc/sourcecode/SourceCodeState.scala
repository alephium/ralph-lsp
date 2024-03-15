package org.alephium.ralph.lsp.pc.sourcecode

import org.alephium.ralph.lsp.access.compiler.RalphParserExtension
import org.alephium.ralph.lsp.access.compiler.ast.Tree
import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage
import org.alephium.ralph.lsp.access.compiler.message.warning.StringWarning
import org.alephium.ralph.lsp.access.util.StringUtil
import org.alephium.ralph.lsp.pc.util.URIUtil
import org.alephium.ralph.{CompiledContract, CompiledScript}

import java.net.URI

sealed trait SourceCodeState {
  def fileURI: URI

  /**
   * Lazily initialise import statements for all files.
   *
   * Can be concurrently accessed or not accessed at all.
   *
   * @see [[URIUtil.importIdentifier]]
   */
  lazy val importIdentifier: Option[Tree.Import] =
    URIUtil
      .importIdentifier(fileURI)
      .flatMap(RalphParserExtension.lazyParseImportIdentifier(_, fileURI))
}

object SourceCodeState {

  implicit val ordering: Ordering[SourceCodeState] =
    Ordering.by[SourceCodeState, URI](_.fileURI)

  /** Represents: Source code exists, but it's neither Compiled or Accessed */
  sealed trait IsInitialised extends SourceCodeState

  /**
   * Represents: Code was accessed. It can either be in Error state or Success state.
   *
   * [[OnDisk]] state is no longer achievable from this state unless the file gets removed/dropped entirely
   * from its source-aware workspace [[org.alephium.ralph.lsp.pc.workspace.WorkspaceState.IsSourceAware]].
   */
  sealed trait IsAccessed extends SourceCodeState

  /** Represents: States where the code text is known */
  sealed trait IsCodeAware extends SourceCodeState {
    def code: String
  }

  /** Represents: Code that is parsed. */
  sealed trait IsParsed extends IsCodeAware

  /** Represents: Code that is compiled. */
  sealed trait IsCompiled extends IsParsed

  /** Represents: Code that contains error(s). */
  sealed trait IsError extends SourceCodeState

  /** Represents: Code that errored during parsing or compilation. */
  sealed trait IsParserOrCompilationError extends IsError with IsCodeAware {
    def errors: Seq[CompilerMessage.AnyError]
  }

  /** The code is on disk */
  case class OnDisk(fileURI: URI) extends IsInitialised

  /** The code is in memory but not parsed or compiled */
  case class UnCompiled(fileURI: URI, code: String) extends IsInitialised with IsAccessed with IsCodeAware

  /** Represents: Was unable to access code */
  case class ErrorAccess(fileURI: URI, error: CompilerMessage.AnyError) extends IsError with IsAccessed

  /** Represents: Code is successfully parsed */
  case class Parsed(fileURI: URI, code: String, ast: Tree.Root) extends IsParsed

  /** Represents: Error during the parser phase. */
  case class ErrorParser(fileURI: URI, code: String, errors: Seq[CompilerMessage.AnyError])
      extends IsParserOrCompilationError
      with IsParsed

  /** Represents: Code is successfully compiled */
  case class Compiled(fileURI: URI,
                      code: String,
                      compiledCode: Seq[Either[CompiledContract, CompiledScript]],
                      parsed: SourceCodeState.Parsed
                     )
      extends IsCompiled {
    def warnings: Seq[StringWarning] =
      compiledCode flatMap {
        case Left(value) =>
          value.warnings map (StringWarning(_, fileURI))

        case Right(value) =>
          value.warnings map (StringWarning(_, fileURI))
      }
  }

  /** Represents: Error during the compilation phase. */
  case class ErrorCompilation(fileURI: URI,
                              code: String,
                              errors: Seq[CompilerMessage.AnyError],
                              parsed: SourceCodeState.Parsed
                             )
      extends IsParserOrCompilationError
      with IsCompiled

}
