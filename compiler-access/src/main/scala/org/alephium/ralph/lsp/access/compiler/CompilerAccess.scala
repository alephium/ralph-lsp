// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler

import org.alephium.ralph._
import org.alephium.ralph.lsp.access.compiler.ast.Tree
import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage
import org.alephium.ralph.lsp.access.compiler.message.error.FastParseError
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST
import org.alephium.ralph.lsp.utils.log.ClientLogger
import org.alephium.ralph.lsp.utils.URIUtil

import java.net.URI

object CompilerAccess {

  val RALPH_FILE_EXTENSION = "ral"

  def ralphc: CompilerAccess =
    RalphCompilerAccess

  /** Checks if the URI is of a `*.ral` source file */
  def isRalphFileExtension(uri: URI): Boolean =
    URIUtil.getFileExtension(uri) == RALPH_FILE_EXTENSION

}

/**
 * Defines functions that perform compiler specific IO operation.
 *
 * @note These functions are mostly in-memory operation and do not
 *       perform file-io.
 * @see [[org.alephium.ralph.lsp.access.file.FileAccess]] for file-io.
 */
trait CompilerAccess {

  /**
   * Runs the soft parser.
   *
   * @param code Code to parse.
   * @return Parsing error or successfully parsed [[SoftAST]].
   */
  def parseSoft(code: String): Either[FastParseError, SoftAST.RootBlock]

  /**
   * Runs the parser phase.
   *
   * @param code Code to parse.
   * @return Parsing error or successfully parsed AST.
   */
  def parseContracts(
      fileURI: URI,
      code: String): Either[CompilerMessage.AnyError, Tree.Root]

  /**
   * Given the parsed AST and compiler options, compile the contracts.
   *
   * @param workspaceErrorURI The [[URI]] to report errors to when `fileURI` is absent from compilation errors.
   */
  def compileContracts(
      parsedSource: Seq[Ast.GlobalDefinition],
      options: CompilerOptions,
      workspaceErrorURI: URI
    )(implicit logger: ClientLogger): Either[CompilerMessage.AnyError, (Array[CompiledContract], Array[CompiledScript], Array[Warning])]

}
