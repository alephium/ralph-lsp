package org.alephium.ralph.lsp.compiler

import org.alephium.ralph.{Ast, CompiledContract, CompiledScript, CompilerOptions}
import org.alephium.ralph.lsp.compiler.message.CompilerMessage
import org.alephium.ralphc.Config

import java.net.URI

object CompilerAccess {
  val RALPH_FILE_EXTENSION = "ral"

  def ralphc: CompilerAccess =
    RalphCompilerAccess
}

/**
 * Defines functions that perform compiler specific IO operation.
 */
trait CompilerAccess {

  /**
   * Checks if a source-file exists.
   *
   * @param fileURI source-file location
   */
  def sourceExists(fileURI: URI): Either[CompilerMessage.AnyError, Boolean]

  /**
   * Fetch all workspace source file locations.
   *
   * @param workspaceURI Project/workspace location.
   */
  def getSourceFiles(workspaceURI: URI): Either[CompilerMessage.AnyError, Seq[URI]]

  /**
   * Fetch the source-code of a file.
   *
   * @param fileURI source-code location.
   */
  def getSourceCode(fileURI: URI): Either[CompilerMessage.AnyError, String]

  /**
   * Runs the parser phase.
   *
   * @param code Code to parse.
   * @return Parsing error or successfully parsed AST.
   */
  def parseContracts(code: String): Either[CompilerMessage.AnyError, Seq[Ast.ContractWithState]]

  /**
   * Given the parsed ast and compiler options, compile the contracts.
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
