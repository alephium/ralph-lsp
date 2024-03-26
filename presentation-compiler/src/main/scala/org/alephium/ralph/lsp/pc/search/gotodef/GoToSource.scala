package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.Ast
import org.alephium.ralph.lsp.access.compiler.ast.Tree
import org.alephium.ralph.lsp.access.compiler.ast.node.Node
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra._
import org.alephium.ralph.lsp.pc.search.gotodef.data.GoToLocation
import org.alephium.ralph.lsp.pc.sourcecode.SourceCodeState
import org.alephium.ralph.lsp.pc.workspace.WorkspaceState

private object GoToSource {

  /**
   * Navigates to the definition of a token in the source code.
   *
   * @param cursorIndex The index of the token clicked by the user.
   * @param sourceCode  The parsed state of the source-code where the search is executed.
   * @param sourceAST   Parsed AST of the searched source file.
   * @return An iterator over the target go-to location(s).
   */
  def goTo(cursorIndex: Int,
           sourceCode: SourceCodeState.Parsed,
           sourceAST: Tree.Source,
           dependencyBuiltIn: Option[WorkspaceState.Compiled]): Iterator[GoToLocation] =
    sourceAST.rootNode.findLast(_.sourceIndex.exists(_ contains cursorIndex)) match { // find the node closest to this source-index
      case Some(closest) =>
        closest match {
          case identNode @ Node(ident: Ast.Ident, _) =>
            // the clicked/closest node is an ident
            GoToIdent.goTo(
              identNode = identNode,
              ident = ident,
              sourceAST = sourceAST,
              sourceCode = sourceCode
            )

          case funcIdNode @ Node(funcId: Ast.FuncId, _) =>
            // the clicked/closest node is functionId
            GoToFuncId.goTo(
              funcIdNode = funcIdNode,
              funcId = funcId,
              source = sourceAST,
              sourceCode = sourceCode,
              dependencyBuiltIn = dependencyBuiltIn
            )

          case typIdNode @ Node(typeId: Ast.TypeId, _) =>
            // the clicked/closest node is TypeId
            GoToTypeId.goTo(
              identNode = typIdNode,
              typeId = typeId,
              source = sourceAST,
              sourceCode = sourceCode
            )

          case _ =>
            Iterator.empty
        }

      case None =>
        Iterator.empty
    }

}
