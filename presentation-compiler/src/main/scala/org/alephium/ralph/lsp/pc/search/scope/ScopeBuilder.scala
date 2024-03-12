package org.alephium.ralph.lsp.pc.search.scope

import org.alephium.ralph.lsp.access.compiler.ast.Tree
import org.alephium.ralph.{Ast, SourceIndex}

object ScopeBuilder {

  /**
   * Builds scoped arguments from a source tree.
   *
   * @param tree The source tree containing the AST.
   * @return A sequence of scope arguments.
   */
  def buildArguments(tree: Tree.Source): Seq[Scope.Argument] =
    tree.ast match {
      case Left(ast) =>
        buildArguments(
          ast = ast,
          astScope = tree.index
        )

      case Right(_) =>
        Seq.empty
    }

  private def buildArguments(ast: Ast.ContractWithState,
                             astScope: SourceIndex): Seq[Scope.Argument] =
    ast match {
      case ast: Ast.TxScript =>
        buildContractArguments(args = ast.templateVars, contractScope = astScope) ++
          buildFunctionArguments(ast.funcs)

      case ast: Ast.Contract =>
        buildContractArguments(args = ast.fields, contractScope = astScope) ++
          buildFunctionArguments(ast.funcs)

      case ast: Ast.ContractInterface =>
        buildFunctionArguments(ast.funcs)
    }

  /** Build scoped arguments from [[Ast.Argument]] */
  private def buildContractArguments(args: Seq[Ast.Argument],
                                     contractScope: SourceIndex): Seq[Scope.Argument] =
    args map {
      argument =>
        Scope.Argument(
          typeDef = argument,
          scope = contractScope
        )
    }

  /** Build scoped arguments from function definition */
  private def buildFunctionArguments(functions: Seq[Ast.FuncDef[_]]): Seq[Scope.Argument] =
    functions flatMap buildArguments

  /** Write scoped function arguments */
  private def buildArguments(function: Ast.FuncDef[_]): Seq[Scope.Argument] =
    function.sourceIndex match {
      case Some(functionScope) =>
        function.args map {
          argument =>
            Scope.Argument(
              typeDef = argument,
              scope = functionScope
            )
        }

      case None =>
        Seq.empty
    }
}
