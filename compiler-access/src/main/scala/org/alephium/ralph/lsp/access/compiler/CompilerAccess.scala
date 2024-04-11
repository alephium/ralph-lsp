package org.alephium.ralph.lsp.access.compiler

import org.alephium.ralph.lsp.access.compiler.ast.Tree
import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage
import org.alephium.ralph.{CompiledScript, Ast, CompilerOptions, CompiledContract}

import java.net.URI

object CompilerAccess {

  val RALPH_FILE_EXTENSION = "ral"

  def ralphc: CompilerAccess =
    RalphCompilerAccess

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
      contracts: Seq[Ast.ContractWithState],
      structs: Seq[Ast.Struct],
      options: CompilerOptions,
      workspaceErrorURI: URI): Either[CompilerMessage.AnyError, (Array[CompiledContract], Array[CompiledScript])]

}
