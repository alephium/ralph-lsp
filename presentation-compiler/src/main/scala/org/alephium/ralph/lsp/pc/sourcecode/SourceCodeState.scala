// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.sourcecode

import org.alephium.ralph.{CompiledContract, CompiledScript}
import org.alephium.ralph.lsp.access.compiler.RalphParserExtension
import org.alephium.ralph.lsp.access.compiler.ast.Tree
import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage
import org.alephium.ralph.lsp.access.compiler.message.error.FastParseError
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST
import org.alephium.ralph.lsp.pc.sourcecode.warning.StringWarning
import org.alephium.ralph.lsp.utils.{LazyVal, URIUtil}

import java.net.URI

sealed trait SourceCodeState {

  def fileURI: URI

  /**
   * Lazily initialise import statements for all files.
   *
   * Can be concurrently accessed or not accessed at all.
   *
   * @return An AST representing this source file's import string literal.
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

  /**
   * Represents: Code that is parsed and compiled
   * and is the co-domain for [[IsParsed]] and [[IsCompiled]].
   */
  sealed trait IsParsedAndCompiled extends IsCodeAware {

    def astSoft: LazyVal[Either[FastParseError, SoftAST.RootBlock]]

  }

  sealed trait IsParsed extends IsParsedAndCompiled

  /** Represents: Code that is compiled. */
  sealed trait IsCompiled extends IsParsedAndCompiled {

    // Compilation can only be executed if a parsed state was successfully created.
    // Therefore, the parsed states is always known during compilation.
    def parsed: SourceCodeState.Parsed

    override def fileURI: URI =
      parsed.fileURI

    override def code: String =
      parsed.code

    override def astSoft: LazyVal[Either[FastParseError, SoftAST.RootBlock]] =
      parsed.astSoft

  }

  /** Represents: Code that contains error(s). */
  sealed trait IsError extends SourceCodeState

  /** Represents: Code that errored during parsing or compilation. */
  sealed trait IsParserOrCompilationError extends IsError with IsCodeAware {

    def errors: Seq[CompilerMessage.AnyError]

  }

  /** The code is on disk */
  case class OnDisk(fileURI: URI) extends IsInitialised

  /** The code is in memory but not parsed or compiled */
  case class UnCompiled(
      fileURI: URI,
      code: String)
    extends IsInitialised
       with IsAccessed
       with IsCodeAware

  /** Represents: Was unable to access code */
  case class ErrorAccess(
      fileURI: URI,
      error: CompilerMessage.AnyError)
    extends IsError
       with IsAccessed

  /** Represents: Code is successfully parsed */
  case class Parsed(
      fileURI: URI,
      code: String,
      astStrict: Tree.Root,
      astSoft: LazyVal[Either[FastParseError, SoftAST.RootBlock]])
    extends IsParsed

  /** Represents: Error during the parser phase. */
  case class ErrorParser(
      fileURI: URI,
      code: String,
      errors: Seq[CompilerMessage.AnyError],
      astSoft: LazyVal[Either[FastParseError, SoftAST.RootBlock]])
    extends IsParserOrCompilationError
       with IsParsed

  /** Represents: Code is successfully compiled */
  case class Compiled(
      compiledCode: Seq[Either[CompiledContract, CompiledScript]],
      parsed: SourceCodeState.Parsed,
      warnings: Seq[StringWarning])
    extends IsCompiled

  /** Represents: Error during the compilation phase. */
  case class ErrorCompilation(
      errors: Seq[CompilerMessage.AnyError],
      parsed: SourceCodeState.Parsed)
    extends IsParserOrCompilationError
       with IsCompiled

}
