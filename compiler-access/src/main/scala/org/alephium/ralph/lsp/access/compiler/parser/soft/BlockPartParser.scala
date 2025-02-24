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
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}

private object BlockPartParser {

  def parseOrFail[Unknown: P](
      blockExpressions: Boolean,
      stop: Token*): P[SoftAST.BlockPartAST] =
    P {
      SpaceParser.parseOrFail |
        TemplateParser.parseOrFail |
        EventParser.parseOrFail |
        StructParser.parseOrFail |
        FunctionParser.parseOrFail |
        ImportParser.parseOrFail |
        InheritanceParser.parseOrFail |
        expression(blockExpressions) |
        CommentParser.parseOrFail |
        UnresolvedParser.parseOrFail(stop)
    }

  private def expression[Unknown: P](blockExpressions: Boolean): P[SoftAST.BlockPartAST] =
    if (blockExpressions)
      ExpressionBlockParser.parseOrFail
    else
      ExpressionParser.parseOrFail

}
