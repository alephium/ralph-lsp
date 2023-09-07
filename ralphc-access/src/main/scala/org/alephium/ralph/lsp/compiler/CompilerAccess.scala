package org.alephium.ralph.lsp.compiler

import org.alephium.ralph.{Ast, CompiledContract, CompiledScript, CompilerOptions}
import org.alephium.ralph.error.CompilerError
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
  def parseCode(code: String): Either[CompilerError.FormattableError, Ast.MultiContract]

  /**
   * Given a parsed workspace returns a compiled workspace.
   */
  def compileContracts(contracts: Seq[Ast.ContractWithState],
                       options: CompilerOptions): Either[FormattableError, (Array[CompiledContract], Array[CompiledScript])]

  /**
   * All files are flushed to disk, this executes compilation
   * accessing files on disk.
   *
   * @return Compiled workspace state that PresentationCompiler can continue with.
   */
  def compileForDeployment(workspaceURI: URI,
                           config: Config): Either[FormattableError, (Array[CompiledContract], Array[CompiledScript])]

}
