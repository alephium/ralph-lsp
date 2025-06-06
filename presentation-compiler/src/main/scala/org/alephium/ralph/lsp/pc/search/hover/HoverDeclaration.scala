// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.hover

import org.alephium.ralph.SourceIndex
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST
import org.alephium.ralph.lsp.pc.sourcecode.{SourceLocation, SourceCodeState}

private case object HoverDeclaration {

  /**
   * Builds the hover content for a given declaration.
   *
   * @param declaration The declaration to build the hover content for.
   * @param sourceCode The source code location where this request was executed.
   * @return An optional hover content if the declaration is supported.
   */
  def apply(
      declaration: SoftAST.DeclarationAST,
      sourceCode: SourceCodeState.IsParsed): Option[SourceLocation.Hover] =
    buildDeclarationContent(declaration).map {
      case (content, index) =>
        SourceLocation.Hover(
          content = content,
          contentPosition = index,
          parsed = sourceCode
        )
    }

  /**
   * Builds the hover content for a given AST declaration.
   *
   * This function checks the type of the declaration and builds the content accordingly.
   *
   * @param declaration The declaration to build the hover content for.
   * @return An optional tuple containing the content and its source index.
   */
  def buildDeclarationContent(declaration: SoftAST.DeclarationAST): Option[(String, SourceIndex)] =
    declaration match {
      case function: SoftAST.Function => Some(buildFunctionContent(function))
      case _: SoftAST.Template        => None
      case _: SoftAST.Enum            => None
      case _: SoftAST.Struct          => None
      case _: SoftAST.Event           => None
      case _: SoftAST.Const           => None
    }

  /**
   * Builds the content for a function declaration.
   *
   * Blocks and post-signature spaces are not included in the content,
   * so their width is subtracted from the function's index width.
   *
   * @param function The function declaration to build the content for.
   * @return A tuple containing the function declaration code and its source index.
   */
  private def buildFunctionContent(function: SoftAST.Function): (String, SourceIndex) = {

    val blockIndexWidth              = function.block.map(_.index.width).getOrElse(0)
    val postSignatureSpaceIndexWidth = function.postSignatureSpace.map(_.index.width).getOrElse(0)

    val index = function.index.copy(width = function.index.width - postSignatureSpaceIndexWidth - blockIndexWidth)

    (function.copy(block = None).toCode(), index)

  }

}
