package org.alephium.ralph.lsp.pc.workspace

import org.alephium.ralph.lsp.pc.compiler.CompilerAccess
import org.alephium.ralph.lsp.pc.sourcecode.{SourceCode, SourceCodeState}
import org.alephium.ralph.CompilerOptions
import org.alephium.ralphc.Config

import scala.util.Try

private[pc] object Workspace {

  def initialise(config: Config): Try[WorkspaceState.UnCompiled] =
    SourceCode
      .initialise(config.contractPath)
      .map(WorkspaceState.UnCompiled(_))

  def parseAndCompile(wsState: WorkspaceState.UnCompiled,
                      compilerOptions: CompilerOptions)(implicit compiler: CompilerAccess): WorkspaceState = {
    val parsed = Workspace.parse(wsState)
    Workspace.compileParsed(parsed, compilerOptions)
  }

  def parse(wsState: WorkspaceState.UnCompiled)(implicit compiler: CompilerAccess): WorkspaceState = {
    val triedParsedStates =
      wsState.sourceCodeStates.map(SourceCode.parse)

    val actualParsedStates =
      triedParsedStates.collect {
        case state: SourceCodeState.Parsed =>
          state

        case code: SourceCodeState.Compiled =>
          code.previousState
      }

    if (actualParsedStates.size != triedParsedStates.size)
      WorkspaceState.UnCompiled(triedParsedStates)
    else
      WorkspaceState.Parsed(actualParsedStates)
  }

  def compileParsed(wsState: WorkspaceState,
                    compilerOptions: CompilerOptions)(implicit compiler: CompilerAccess): WorkspaceState =
    wsState match {
      case state: WorkspaceState.UnCompiled =>
        state

      case state: WorkspaceState.Compiled =>
        state

      case state: WorkspaceState.Parsed =>
        compiler.compileWorkspace(state, compilerOptions)
    }

}
