// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.range
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}

object StructDeconstructorFieldParser {

  def parseOrFail[Unknown: P]: P[SoftAST.StructDeconstructorField] =
    P {
      Index ~
        leftExpression ~
        parseOrFailReference.? ~
        Index
    } map {
      case (from, left, reference, to) =>
        SoftAST.StructDeconstructorField(
          index = range(from, to),
          expressionLeft = left,
          reference = reference
        )
    }

  /**
   * In a [[SoftAST.StructDeconstructorField]], the right-expression is optional.
   *
   * For example, in the following case, `field2` is redeclared as a new local variable, whereas `field1` remains a local variable.
   * {{{
   *   let MyStruct { field1, field2: >>reference<< }
   * }}}
   *
   * This parser parses just that tail code, including the colon:
   *
   * {{{
   *   let MyStruct { field1, field2 : reference }
   *                                 ↑_________↑
   * }}}
   */
  private def parseOrFailReference[Unknown: P]: P[SoftAST.StructDeconstructorFieldReference] =
    P {
      Index ~
        SpaceParser.parseOrFail.? ~
        TokenParser.parseOrFail(Token.Colon) ~
        SpaceParser.parseOrFail.? ~
        ExpressionParser.parseSubset(rightExpression) ~
        Index
    } map {
      case (from, postIdentifierSpace, equalToken, postEqualSpace, right, to) =>
        SoftAST.StructDeconstructorFieldReference(
          index = range(from, to),
          preColonSpace = postIdentifierSpace,
          colon = equalToken,
          preExpressionSpace = postEqualSpace,
          expressionRight = right
        )
    }

  private def leftExpression[Unknown: P] =
    P {
      MutableBindingParser.parseOrFail |
        IdentifierParser.parseOrFail
    }

  private def rightExpression[Unknown: P] =
    P {
      StructDeconstructorParser.parseOrFail |
        MutableBindingParser.parseOrFail |
        IdentifierParser.parseOrFail
    }

}
