package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.{point, range}
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}

private object TextParser {

  /**
   * Parses a text string, stopping at the specified tokens.
   *
   * @param stop The tokens at which parsing should stop.
   * @return One of the following:
   *         - [[SoftAST.CodeStringExpected]] if the text is empty.
   *         - [[SoftAST.CodeString]] for non-empty text.
   */
  def parse[Unknown: P](stop: Token*): P[SoftAST.CodeStringAST] =
    P(Index ~ parseOrFail(stop: _*).?) map {
      case (from, None) =>
        SoftAST.CodeStringExpected(point(from))

      case (_, Some(code)) =>
        code
    }

  /**
   * Parses a text string, stopping at the specified tokens.
   *
   * @param stop The tokens at which parsing should stop.
   * @return Non-empty text string.
   */
  def parseOrFail[Unknown: P](stop: Token*): P[SoftAST.CodeString] =
    P(Index ~ TokenParser.WhileNotOrFail(stop).! ~ Index) map {
      case (from, text, to) =>
        SoftAST.CodeString(
          text = text,
          index = range(from, to)
        )
    }

}
