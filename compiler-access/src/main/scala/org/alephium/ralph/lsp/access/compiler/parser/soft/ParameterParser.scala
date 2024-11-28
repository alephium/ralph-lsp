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
import org.alephium.ralph.lsp.access.compiler.parser.soft.CommonParser._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST

private object ParameterParser {

  def parse[Unknown: P]: P[SoftAST.ParameterClauseAST] =
    P(emptyParams | atLeastOneParam)

  private def emptyParams[Unknown: P]: P[SoftAST.EmptyParameterClause] =
    P(Index ~ TokenParser.openParenOrFail ~ spaceOrFail.? ~ TokenParser.closeParenOrFail ~ Index) map {
      case (from, openParen, space, closeParen, to) =>
        SoftAST.EmptyParameterClause(
          index = range(from, to),
          openParen = openParen,
          preCloseParenSpace = space,
          closeParen = closeParen
        )
    }

  private def atLeastOneParam[Unknown: P]: P[SoftAST.NonEmptyParameterClause] =
    P(Index ~ TokenParser.openParen ~ spaceOrFail.? ~ oneParameter ~ tailParam.rep ~ spaceOrFail.? ~ TokenParser.closeParen ~ Index) map {
      case (from, openParen, headSpace, headParam, tailParams, tailSpace, closeParen, to) =>
        SoftAST.NonEmptyParameterClause(
          index = range(from, to),
          openParen = openParen,
          preParamsSpace = headSpace,
          headParam = headParam,
          tailParams = tailParams,
          postParamSpace = tailSpace,
          closeParen = closeParen
        )
    }

  private def oneParameter[Unknown: P]: P[SoftAST.Parameter] =
    P(Index ~ identifier ~ spaceOrFail.? ~ TokenParser.colon ~ spaceOrFail.? ~ TypeParser.parse ~ Index) map {
      case (from, paramName, headSpace, colon, tailSpace, typeName, to) =>
        SoftAST.Parameter(
          index = range(from, to),
          paramName = paramName,
          preColonSpace = headSpace,
          colon = colon,
          postColonSpace = tailSpace,
          paramType = typeName
        )
    }

  private def tailParam[Unknown: P]: P[SoftAST.TailParameter] =
    P(Index ~ spaceOrFail.? ~ TokenParser.comma ~ spaceOrFail.? ~ oneParameter ~ Index) map {
      case (from, headSpace, comma, tailSpace, param, to) =>
        SoftAST.TailParameter(
          index = range(from, to),
          preCommaSpace = headSpace,
          comma = comma,
          postCommaSpace = tailSpace,
          parameter = param
        )
    }

}
