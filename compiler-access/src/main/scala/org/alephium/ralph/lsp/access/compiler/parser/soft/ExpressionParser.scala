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
      ImportParser.parseOrFail |
        TypeAssignmentParser.parseOrFail |
        AssignmentParser.parseOrFail |
        InfixCallParser.parseOrFail |
        MethodCallParser.parseOrFail |
        BlockParser.parseOrFail |
        ReturnStatementParser.parseOrFail |
        ForLoopParser.parseOrFail |
        WhileLoopParser.parseOrFail |
        VariableDeclarationParser.parseOrFail |
        MutableBindingParser.parseOrFail |
        ReferenceCallParser.parseOrFail |
        AnnotationParser.parseOrFail |
        ParameterParser.parseOrFail |
        NumberParser.parseOrFail |
        BooleanParser.parseOrFail |
        BStringParser.parseOrFail |
        StringLiteralParser.parseOrFail |
        IdentifierParser.parseOrFail
    }

}
