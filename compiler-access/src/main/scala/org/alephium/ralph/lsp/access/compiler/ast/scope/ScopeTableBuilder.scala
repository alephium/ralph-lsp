package org.alephium.ralph.lsp.access.compiler.ast.scope

import org.alephium.ralph.{Ast, SourceIndex}

object ScopeTableBuilder {

  /**
   * Creates a [[ScopeTable]] instance for the given AST.
   *
   * @param ast      The AST to use for creating the [[ScopeTable]].
   * @param astScope The scope of the AST.
   * @return A [[ScopeTable]] instance.
   */
  def build(ast: Ast.ContractWithState,
            astScope: SourceIndex): ScopeTable = {
    val table =
      ScopeTable()

    ast match {
      case Ast.TxScript(ident, templateVars, funcs) =>
        putArguments(args = templateVars, contractScope = astScope, table = table)
        putFunctions(funcs, contractScope = astScope, table = table)

      case Ast.Contract(stdIdEnabled, stdInterfaceId, isAbstract, ident, templateVars, fields, funcs, events, constantVars, enums, inheritances) =>
        putArguments(args = fields, contractScope = astScope, table = table)
        putFunctions(funcs, contractScope = astScope, table = table)

      case Ast.ContractInterface(stdId, ident, funcs, events, inheritances) =>
        putFunctions(funcs, contractScope = astScope, table = table)
    }

    table
  }

  /** Write scoped arguments */
  private def putArguments(args: Seq[Ast.Argument],
                           contractScope: SourceIndex,
                           table: ScopeTable): Unit =
    args foreach {
      argument =>
        table.putArgument(
          name = argument.ident.name,
          namedScope =
            Scope.Argument(
              typeDef = argument,
              scope = contractScope
            )
        )
    }

  /** Write scoped functions */
  private def putFunctions(functions: Seq[Ast.FuncDef[_]],
                           contractScope: SourceIndex,
                           table: ScopeTable): Unit =
    functions foreach {
      function =>
        table.putFunction(
          name = function.id.name,
          namedScope =
            Scope.Function(
              funcDef = function,
              scope = contractScope
            )
        )

        putFunctionsArguments(
          function = function,
          table = table
        )
    }

  /** Write scoped function arguments */
  private def putFunctionsArguments(function: Ast.FuncDef[_],
                                    table: ScopeTable): Unit =
    function.sourceIndex foreach {
      functionScope =>
        function.args foreach {
          argument =>
            table.putArgument(
              name = argument.ident.name,
              namedScope =
                Scope.Argument(
                  typeDef = argument,
                  scope = functionScope
                )
            )
        }
    }
}
