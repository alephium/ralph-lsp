// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.range
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST

private object ExpressionParser {

  def parse[Unknown: P]: P[SoftAST.ExpressionAST] =
    parseSubset(parseOrFail)

  /**
   * Attempts to execute a subset of expression parsers.
   *
   * @param parseOrFail The parser to execute.
   * @return A successfully parsed expression, or a [[SoftAST.ExpressionExpected]] error.
   */
  def parseSubset[Unknown: P](parseOrFail: => P[SoftAST.ExpressionAST]): P[SoftAST.ExpressionAST] =
    P {
      Index ~
        parseOrFail.? ~
        Index
    } map {
      case (_, Some(expression), _) =>
        expression

      case (from, None, to) =>
        SoftAST.ExpressionExpected(range(from, to))
    }

  def parseOrFail[Unknown: P]: P[SoftAST.ExpressionAST] =
    P {
      MapAssignmentParser.parseOrFail |
        TypeAssignmentParser.parseOrFail |
        AssignmentParser.parseOrFail |
        InfixCallParser.parseOrFail |
        MethodCallParser.parseOrFail |
        BlockParser.parseOrFail |
        ReturnParser.parseOrFail |
        ForParser.parseOrFail |
        WhileParser.parseOrFail |
        VariableDeclarationParser.parseOrFail |
        MutableBindingParser.parseOrFail |
        IfElseParser.parseOrFail |
        ElseParser.parseOrFail |
        EmitParser.parseOrFail |
        ReferenceCallParser.parseOrFail |
        StructConstructorParser.parseOrFail |
        AnnotationParser.parseOrFail |
        TupleParser.parseOrFail(assertNonEmpty = false) |
        ArrayParser.parseOrFail |
        ByteVecParser.parseOrFail |
        NumberParser.parseOrFail |
        BooleanParser.parseOrFail |
        AlphParser.parseOrFail |
        BStringParser.parseOrFail |
        StringInterpolationParser.parseOrFail |
        StringLiteralParser.parseOrFail |
        ArrayAccessParser.parseOrFail |
        IdentifierParser.parseOrFail
    }

}
