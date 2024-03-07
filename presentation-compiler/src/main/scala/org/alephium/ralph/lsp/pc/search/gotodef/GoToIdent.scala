package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.Ast
import org.alephium.ralph.Ast.Positioned
import org.alephium.ralph.lsp.access.compiler.ast.Tree
import org.alephium.ralph.lsp.access.compiler.ast.node.Node
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.SourceIndexExtension
import org.alephium.ralph.lsp.pc.search.scope.ScopeBuilder

import scala.collection.immutable.ArraySeq

private object GoToIdent {

  /**
   * Navigate to the nearest argument for the given identifier.
   *
   * @param identNode The node representing the identifier in the AST.
   * @param ident     The identifier for which the argument definition is sought.
   * @param source    The source tree to search within.
   * @return An option containing the closest argument if found, otherwise None.
   * */
  def goTo(identNode: Node[Positioned],
           ident: Ast.Ident,
           source: Tree.Source): ArraySeq[Ast.Argument] =
    identNode
      .parent // take one step up to check the type of ident node.
      .map(_.data)
      .to(ArraySeq)
      .collect {
        case variable: Ast.Variable[_] if variable.id == ident => // Is it a variable?
          // The user clicked on a variable. Take 'em there!
          goToVariable(
            variableNode = identNode,
            variable = variable,
            source = source
          )
      }
      .flatten

  /** Navigate to the nearest argument for the given variable.
   *
   * @param variableNode The node representing the variable.
   * @param variable     The variable to find the argument for.
   * @param source       The source tree to search within.
   * @return An option containing the closest argument if found, otherwise None.
   * */
  private def goToVariable(variableNode: Node[Positioned],
                           variable: Ast.Variable[_],
                           source: Tree.Source): ArraySeq[Ast.Argument] =
    variable
      .sourceIndex
      .to(ArraySeq)
      .flatMap {
        variableIndex =>
          ScopeBuilder
            .buildArguments(source)
            .filter {
              argument =>
                argument.typeDef.ident == variable.id &&
                  argument.scope.contains(variableIndex.index)
            }
            .map(_.typeDef)
      }
}
