// Copyright 2024 The Alephium Authors
// This file is part of the alephium project.
//
// The library is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// The library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with the library. If not, see http://www.gnu.org/licenses/.

package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.range
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST

private object ExpressionParser {

  def parse[Unknown: P]: P[SoftAST.ExpressionAST] =
    parseSelective(
      parseInfix = true,
      parseMethodCall = true
    )

  def parseOrFail[Unknown: P]: P[SoftAST.ExpressionAST] =
    parseOrFailSelective(
      parseInfix = true,
      parseMethodCall = true
    )

  def parseSelective[Unknown: P](
      parseInfix: Boolean,
      parseMethodCall: Boolean): P[SoftAST.ExpressionAST] =
    P {
      Index ~
        parseOrFailSelective(parseInfix = parseInfix, parseMethodCall = parseMethodCall).? ~
        Index
    } map {
      case (_, Some(expression), _) =>
        expression

      case (from, None, to) =>
        SoftAST.ExpressionExpected(range(from, to))
    }

  def parseOrFailSelective[Unknown: P](
      parseInfix: Boolean,
      parseMethodCall: Boolean): P[SoftAST.ExpressionAST] = {
    def infixOrFail() =
      if (parseInfix)
        InfixCallParser.parseOrFail
      else
        Fail(s"${InfixCallParser.productPrefix} ignored")

    def methodCallOrFail() =
      if (parseMethodCall)
        MethodCallParser.parseOrFail
      else
        Fail(s"${MethodCallParser.productPrefix} ignored")

    P {
      infixOrFail() |
        methodCallOrFail() |
        common
    }
  }

  private def common[Unknown: P]: P[SoftAST.ExpressionAST] =
    P {
      ReturnStatementParser.parseOrFail |
        ForLoopParser.parseOrFail |
        WhileLoopParser.parseOrFail |
        VariableDeclarationParser.parseOrFail |
        AssignmentParser.parseOrFail |
        TypeAssignmentParser.parseOrFail |
        BlockParser.clause(required = false) |
        ReferenceCallParser.parseOrFail |
        AnnotationParser.parseOrFail |
        TupleParser.parseOrFail |
        NumberParser.parseOrFail |
        BooleanParser.parseOrFail |
        IdentifierParser.parseOrFail
    }

}
