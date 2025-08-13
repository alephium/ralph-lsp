// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.Ast
import org.alephium.ralph.lsp.pc.sourcecode.SourceLocation
import org.alephium.ralph.lsp.utils.log.StrictImplicitLogging

private[search] object GoToDefIdent extends StrictImplicitLogging {

  /**
   * Navigate to the template argument(s) for the given identifier.
   *
   * @param ident      The variable identifier to find arguments for.
   * @param sourceCode The source tree to search within.
   * @return An array sequence of [[Ast.Argument]]s matching the search result.
   */
  def goToTemplateArguments(
      ident: Ast.Ident,
      sourceCode: SourceLocation.CodeStrict): Seq[SourceLocation.NodeStrict[Ast.Ident]] = {
    val arguments =
      sourceCode.tree.ast match {
        case ast: Ast.TxScript =>
          ast.templateVars

        case contract: Ast.Contract =>
          contract.templateVars ++ contract.fields

        case _: Ast.ContractInterface | _: Ast.Struct | _: Ast.EnumDef[_] | _: Ast.ConstantVarDef[_] | _: Ast.AssetScript =>
          Seq.empty
      }

    arguments
      .filter(_.ident == ident)
      .map {
        argument =>
          SourceLocation.NodeStrict(
            ast = argument.ident,
            source = sourceCode
          )
      }
  }

}
