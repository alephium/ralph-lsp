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

private object TypeParser {

  def parse[Unknown: P]: P[SoftAST.TypeAST] =
    P(tupledTypeNames | typeName)

  private def tupledTypeNames[Unknown: P]: P[SoftAST.TupledType] =
    P(Index ~ TokenParser.openParenOrFail ~ spaceOrFail.? ~ parse ~ commaTypeName.rep ~ TokenParser.closeParen ~ Index) map {
      case (from, openParen, preHeadTypeSpace, headType, tailTypes, closeParen, to) =>
        SoftAST.TupledType(
          index = range(from, to),
          openParen = openParen,
          preHeadTypeSpace = preHeadTypeSpace,
          headType = headType,
          tailTypes = tailTypes,
          closeParen = closeParen
        )
    }

  private def commaTypeName[Unknown: P]: P[SoftAST.TailType] =
    P(Index ~ TokenParser.commaOrFail ~ spaceOrFail.? ~ parse ~ spaceOrFail.? ~ Index) map {
      case (from, comma, preTypeNameSpace, typeName, postTypeNameSpace, to) =>
        SoftAST.TailType(
          index = range(from, to),
          comma = comma,
          preTypeNameSpace = preTypeNameSpace,
          tpe = typeName,
          postTypeNameSpace = postTypeNameSpace
        )
    }

}
