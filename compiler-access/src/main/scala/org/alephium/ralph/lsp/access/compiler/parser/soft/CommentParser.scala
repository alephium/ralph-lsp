// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.range
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
        SpaceParser.parseOrFail.? ~
        one.rep(1) ~
        // read the tail space so that the next token/expression is parsable
        SpaceParser.parseOrFail.? ~
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
        SpaceParser.parseOrFailSingleLine.? ~
        TextParser.parseOrFail(Token.Newline).? ~
        // read the tail space only if the next line is also a comment
        (SpaceParser.parseOrFail ~ &(TokenParser.parseOrFailUndocumented(Token.DoubleForwardSlash))).? ~
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
