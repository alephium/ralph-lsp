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
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}

private object CommentParser {

  /**
   * Parses multi-line comments.
   *
   * Syntax: `[Space][Comments...][Space]`
   *
   * This parser is unique as it handles both the spaces before (`preSpace`)
   * and after (`postSpace`) the comment syntax, unlike other parsers that
   * ignore spaces before the initial syntax. In the case of comments,
   * the initial syntax is `//`.
   *
   * @return A parsed representation of comments as a [[SoftAST.Comments]] object.
   */
  def parseOrFail[Unknown: P]: P[SoftAST.Comments] =
    P {
      Index ~
        spaceOrFail.? ~
        one.rep(1) ~
        spaceOrFail.? ~
        Index
    } map {
      case (from, preSpace, comments, postSpace, to) =>
        SoftAST.Comments(
          index = range(from, to),
          preCommentSpace = preSpace,
          comments = comments,
          postCommentSpace = postSpace
        )
    }

  /**
   * Parses single-line comments.
   *
   * Syntax: `[//][Space][Text][Space]`
   *
   * @return A parsed representation of a single-line comment as a [[SoftAST.Comment]] object.
   */
  private def one[Unknown: P]: P[SoftAST.Comment] =
    P {
      Index ~
        TokenParser.parseOrFailUndocumented(Token.DoubleForwardSlash) ~
        spaceOrFail.? ~
        text.? ~
        spaceOrFail.? ~
        Index
    } map {
      case (from, doubleForwardSlash, preTextSpace, text, postTextSpace, to) =>
        SoftAST.Comment(
          index = range(from, to),
          doubleForwardSlash = doubleForwardSlash,
          preTextSpace = preTextSpace,
          text = text,
          postTextSpace = postTextSpace
        )
    }

}
