// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}

private object IdentifierParser {

  def parse[Unknown: P](required: Boolean): P[SoftAST.IdentifierAST] =
    if (required)
      parse
    else
      parseOrFail

  def parse[Unknown: P]: P[SoftAST.IdentifierAST] =
    P(Index ~ parseOrFail.?) map {
      case (_, Some(identifier)) =>
        identifier

      case (from, None) =>
        SoftAST.IdentifierExpected(point(from))
    }

  /**
   * Parses identifiers as long as they are not reserved tokens [[Token.Reserved]].
   *
   * For example, the following code will result in an [[SoftAST.IdentifierExpected]] error
   * because `let` is a reserved token [[Token.Let]]:
   *
   * {{{
   *    fn let() -> ()
   * }}}
   *
   * @return A successfully parsed identifier instance [[SoftAST.Identifier]] or a parser error.
   */
  def parseOrFail[Unknown: P]: P[SoftAST.Identifier] =
    P {
      Index ~
        CommentParser.parseOrFail.? ~
        // disallow reserved names such as `let mut = 1`.
        // also handle cases where tail is the end of file `let mut`.
        !(TokenParser.Reserved() ~ ((SpaceParser.parseOrFail: P[Unit]) | End)) ~
        CodeParser.parseOrFail(isDevDefinedName.!) ~
        Index
    } map {
      case (from, documentation, code, to) =>
        SoftAST.Identifier(
          index = range(from, to),
          code = code,
          documentation = documentation
        )
    }

  private def isDevDefinedName[Unknown: P]: P[Unit] =
    CharsWhile {
      char =>
        char.isLetterOrDigit || Token.Underscore.lexeme.contains(char) || Token.Exclamation.lexeme.contains(char)
    }

}
