package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.Ast
import org.alephium.ralph.lsp.access.compiler.ast.Tree
import org.alephium.ralph.lsp.access.compiler.ast.node.Node
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.SourceIndexExtension
import org.alephium.ralph.lsp.pc.search.gotodef.data.GoToLocation
import org.alephium.ralph.lsp.pc.sourcecode.SourceCodeState

import scala.collection.immutable.ArraySeq

object GoToSource {

  /**
   * Go to definition for source-code.
   *
   * @param cursorIndex The Requested index (token clicked by user).
   * @param sourceCode  The requested file.
   * @param sourceAST   Parsed AST of the requested source file's code.
   * @return Target go-to location.
   */
  def goTo(cursorIndex: Int,
           sourceCode: SourceCodeState.Parsed,
           sourceAST: Tree.Source): ArraySeq[GoToLocation] = {
    val goToResult =
      goTo(
        cursorIndex = cursorIndex,
        source = sourceAST
      )

    // covert go-to node to GoToLocation
    GoToLocation(
      sourceCode = sourceCode,
      ast = goToResult.to(ArraySeq)
    )
  }

  private def goTo(cursorIndex: Int,
                   source: Tree.Source): Option[Ast.Positioned] =
    source
      .rootNode
      .findLast(_.sourceIndex.exists(_ contains cursorIndex)) // find the node closest to this source-index
      .collect {
        case identNode @ Node(ident: Ast.Ident, _) =>
          // the clicked/closest node is an ident
          GoToIdent.goTo(
            identNode = identNode,
            ident = ident,
            source = source
          )

        case funcIdNode @ Node(funcId: Ast.FuncId, _) =>
          // the clicked/closest node is functionId
          GoToFuncId.goTo(
            funcIdNode = funcIdNode,
            funcId = funcId,
            source = source
          )
      }
      .flatten

}
