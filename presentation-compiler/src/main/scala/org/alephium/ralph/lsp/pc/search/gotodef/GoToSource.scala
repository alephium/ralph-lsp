package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.Ast
import org.alephium.ralph.lsp.access.compiler.ast.node.Node
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra._
import org.alephium.ralph.lsp.pc.search.gotodef.data.GoToLocation
import org.alephium.ralph.lsp.pc.sourcecode.SourceTreeInScope
import org.alephium.ralph.lsp.pc.workspace.WorkspaceState
import org.alephium.ralph.lsp.pc.workspace.build.dependency.DependencyID

private object GoToSource {

  /**
   * Navigates to the definition of a token in the source code.
   *
   * @param cursorIndex The index of the token clicked by the user.
   * @param sourceCode  The parsed state of the source-code where the search is executed.
   * @return An iterator over the target go-to location(s).
   */
  def goTo(cursorIndex: Int,
           sourceCode: SourceTreeInScope,
           workspace: WorkspaceState.IsSourceAware): Iterator[GoToLocation] =
    sourceCode.tree.rootNode.findLast(_.sourceIndex.exists(_ contains cursorIndex)) match { // find the node closest to this source-index
      case Some(closest) =>
        closest match {
          case identNode @ Node(ident: Ast.Ident, _) =>
            // the clicked/closest node is an ident
            GoToIdent.goTo(
              identNode = identNode,
              ident = ident,
              sourceCode = sourceCode,
              workspace = workspace
            )

          case funcIdNode @ Node(funcId: Ast.FuncId, _) =>
            // the clicked/closest node is functionId
            GoToFuncId.goTo(
              funcIdNode = funcIdNode,
              funcId = funcId,
              source = sourceCode.tree,
              sourceCode = sourceCode.parsed,
              dependencyBuiltIn = workspace.build.findDependency(DependencyID.BuiltIn)
            )

          case typIdNode @ Node(typeId: Ast.TypeId, _) =>
            // the clicked/closest node is TypeId
            GoToTypeId.goTo(
              identNode = typIdNode,
              typeId = typeId,
              source = sourceCode.tree,
              sourceCode = sourceCode.parsed
            )

          case _ =>
            Iterator.empty
        }

      case None =>
        Iterator.empty
    }

}
