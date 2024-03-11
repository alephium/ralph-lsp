package org.alephium.ralph.lsp.access.compiler

import org.alephium.ralph.lsp.access.compiler.ast.Tree
import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage
import org.alephium.ralph.{Ast, CompiledContract, CompiledScript, CompilerOptions}
import org.alephium.ralphc.Config

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
  def parseContracts(fileURI: URI, code: String): Either[CompilerMessage.AnyError, Tree.Root]

  /**
   * Given the parsed AST and compiler options, compile the contracts.
   */
  def compileContracts(contracts: Seq[Ast.ContractWithState],
                       options: CompilerOptions): Either[CompilerMessage.AnyError, (Array[CompiledContract], Array[CompiledScript])]

  /**
   * Compile the entire workspace from disk and prepare for deployment.
   *
   * Prerequisite: All files are flushed to disk
   */
  def compileForDeployment(workspaceURI: URI,
                           config: Config): Either[CompilerMessage.AnyError, (Array[CompiledContract], Array[CompiledScript])]

}
