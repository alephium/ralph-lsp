package org.alephium.ralph.lsp.compiler

import org.alephium.ralph.{Ast, CompiledContract, CompiledScript, CompilerOptions}
import org.alephium.ralph.error.CompilerError.FormattableError
import org.alephium.ralphc.Config

import java.net.URI

object CompilerAccess {
  def ralphc: CompilerAccess =
    RalphCompilerAccess
}

/**
 * Functions required by PresentationCompiler.
 */
trait CompilerAccess {

  /**
   * Runs the parser phase.
   *
   * @param code the code to parse.
   * @return An error or the successfully parsed AST.
   */
  def parseContracts(code: String): Either[FormattableError, Seq[Ast.ContractWithState]]

  /**
   * Given parsed ast and compiler options, compile the contracts.
   */
  def compileContracts(contracts: Seq[Ast.ContractWithState],
                       options: CompilerOptions): Either[FormattableError, (Array[CompiledContract], Array[CompiledScript])]

  /**
   * All files are flushed to disk. Compile workspace from disk to prepare for deployment.
   */
  def compileForDeployment(workspaceURI: URI,
                           config: Config): Either[FormattableError, (Array[CompiledContract], Array[CompiledScript])]

}
