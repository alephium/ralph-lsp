// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.{point, range}
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}

private object StringLiteralParser {

  /**
   * Parses text enclosed in quotes, e.g. `"some text"`.
   *
   * If the text follows a path format (i.e., it contains forward slashes [[Token.ForwardSlash]]),
   * it is parsed as a path.
   */
  def parseOrFail[Unknown: P]: P[SoftAST.StringLiteral] =
    P {
      Index ~
        TokenParser.parseOrFail(Token.Quote) ~
        Index ~
        // TODO: Other keywords such as `Contract`, `Abstract` etc might also need stopping here.
        //       Add them only if LSP features require them.
        TextParser.parseOrFail(Token.ForwardSlash, Token.Quote, Token.Import).? ~
        path.rep ~
        TokenParser.parse(Token.Quote) ~
        Index
    } map {
      case (from, startQuote, headIndex, head, tail, endQuote, to) =>
        val headResult =
          if (head.isEmpty && tail.nonEmpty)
            // if the tail-path is provided, e.g. `import "/abc"` but the head-path is empty,
            // report error at the head-path.
            Some(SoftAST.CodeStringExpected(point(headIndex)))
          else
            head

        SoftAST.StringLiteral(
          index = range(from, to),
          startQuote = startQuote,
          head = headResult,
          tail = tail,
          endQuote = endQuote
        )
    }

  /** A string-literal could also define a path if it contains forward slashes */
  private def path[Unknown: P]: P[SoftAST.Path] =
    P {
      Index ~
        TokenParser.parseOrFail(Token.ForwardSlash) ~
        TextParser.parse(Token.ForwardSlash, Token.Quote, Token.Import) ~
        Index
    } map {
      case (from, slash, text, to) =>
        SoftAST.Path(
          index = range(from, to),
          slash = slash,
          text = text
        )
    }

}
