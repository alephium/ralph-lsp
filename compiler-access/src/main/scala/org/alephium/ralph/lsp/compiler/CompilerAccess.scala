package org.alephium.ralph.lsp.compiler

import org.alephium.ralph.{Ast, CompiledContract, CompiledScript, CompilerOptions}
import org.alephium.ralph.error.CompilerError.FormattableError
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
   * Fetch all workspace source file locations.
   *
   * @param workspaceURI Project/workspace location.
   */
  def getSourceFiles(workspaceURI: URI): Either[FormattableError, Seq[URI]]

  /**
   * Fetch the source-code of a file.
   *
   * @param fileURI source-code location.
   */
  def getSourceCode(fileURI: URI): Either[FormattableError, String]

  /**
   * Runs the parser phase.
   *
   * @param code Code to parse.
   * @return Parsing error or successfully parsed AST.
   */
  def parseContracts(code: String): Either[FormattableError, Seq[Ast.ContractWithState]]

  /**
   * Given the parsed ast and compiler options, compile the contracts.
   */
  def compileContracts(contracts: Seq[Ast.ContractWithState],
                       options: CompilerOptions): Either[FormattableError, (Array[CompiledContract], Array[CompiledScript])]

  /**
   * Compile the entire workspace from disk and prepare for deployment.
   *
   * Prerequisite: All files are flushed to disk
   */
  def compileForDeployment(workspaceURI: URI,
                           config: Config): Either[FormattableError, (Array[CompiledContract], Array[CompiledScript])]

}
