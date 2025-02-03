package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.range
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}

private object ImportParser {

  /**
   * Parses an import statement.
   *
   * Example syntax:
   *
   * {{{
   *   import "some text"
   *   import "a/b/c/d"
   * }}}
   */
  def parseOrFail[Unknown: P]: P[SoftAST.Import] =
    P {
      Index ~
        TokenParser.parseOrFail(Token.Import) ~
        TokenParser.isBoundary(Token.Quote) ~
        SpaceParser.parseOrFail.? ~
        StringLiteralParser.parseOrFail.? ~
        Index
    } map {
      case (from, importToken, space, endQuote, to) =>
        SoftAST.Import(
          index = range(from, to),
          importToken = importToken,
          postImportSpace = space,
          string = endQuote
        )
    }

}
