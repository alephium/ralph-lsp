// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.range
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST

private object ExpressionBlockParser {

  /**
   * Parses two or more expressions as a block.
   *
   * For example, when two or more expressions are defined within a root call:
   * {{{
   *   let one = 1
   *   let two = 2
   * }}}
   *
   * These expressions should be parsed such that they are available within a single tree.
   * The AST [[SoftAST.ExpressionBlock]] provides this tree. But if these expressions
   * are already defined within a parent block, for example, within a contract or a function,
   * then the expression-block is not required.
   *
   * @return An expression-block when multiple expressions are defined, otherwise a single expression-ast.
   */
  def parseOrFail[Unknown: P]: P[SoftAST.BlockPartAST] =
    P {
      Index ~
        ExpressionParser.parseOrFail ~
        tail.rep(1).? ~
        Index
    } map {
      case (from, headExpression, Some(tailExpressions), to) =>
        SoftAST.ExpressionBlock(
          index = range(from, to),
          headExpression = headExpression,
          tailExpressions = tailExpressions
        )

      case (_, headExpression, None, _) =>
        headExpression
    }

  /**
   * Optional space because the previous expression could've parsed the space due to missing expected expressions.
   *
   * For example:
   * {{{
   *   let >>varA<< // missing `=` and `rightExpression` but the space is parsed looking for these missing expressions.
   *   let copy = var@@A
   * }}}
   */
  private def tail[Unknown: P]: P[SoftAST.TailExpressionBlock] =
    P {
      Index ~
        SpaceParser.parseOrFail.? ~
        ExpressionParser.parseOrFail ~
        Index
    } map {
      case (from, space, ast, to) =>
        SoftAST.TailExpressionBlock(
          index = range(from, to),
          preExpressionSpace = space,
          expression = ast
        )
    }

}
