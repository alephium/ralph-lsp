package org.alephium.ralph.lsp.pc.semantic

import org.alephium.ralph.lsp.access.compiler.ast.Tree
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra._
import org.alephium.ralph.lsp.pc.log.{ClientLogger, StrictImplicitLogging}
import org.alephium.ralph.lsp.pc.sourcecode.SourceCodeState
import org.alephium.ralph.lsp.pc.workspace.WorkspaceState
import org.alephium.ralph.lsp.pc.workspace.build.dependency.DependencyID

import java.net.URI

object SemanticTokensProvider extends StrictImplicitLogging {

  def provide(
    fileURI:URI,
                      workspace: WorkspaceState.IsSourceAware)(implicit logger: ClientLogger): Iterator[Int] ={
                        Iterator(1,1,1,1,1)
                      }
}

