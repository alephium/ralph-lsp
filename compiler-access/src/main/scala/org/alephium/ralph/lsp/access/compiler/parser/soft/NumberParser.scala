package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}

private object NumberParser {

  def parseOrFail[Unknown: P]: P[SoftAST.Number] =
    P {
      Index ~
        CommentParser.parseOrFail.? ~
        number ~
        unitAlph.? ~
        Index
    } map {
      case (from, documentation, digits, unit, to) =>
        SoftAST.Number(
          index = range(from, to),
          documentation = documentation,
          number = digits,
          unit = unit
        )
    }

  private def number[Unknown: P]: P[SoftAST.CodeString] =
    P {
      Index ~
        numberOrHex.! ~
        Index
    } map {
      case (from, number, to) =>
        SoftAST.CodeString(
          index = range(from, to),
          text = number
        )
    }

  /**
   * Parses characters that <u><b>could</b></u> (not strict) form a number.
   *
   * Follows rules defined by the parser [[org.alephium.ralph.Lexer.integer]].
   * This is to handle all cases that are compiled successfully in ralphc.
   *
   * For example, the following are valid numbers in ralphc:
   *
   * {{{
   *   let num0 = 1_
   *   let num1 = 1_._
   *   let num2 = 1_.____0
   *   let num3 = 1_._0___0
   * }}}
   *
   * Current parser code in [[org.alephium.ralph.Lexer.integer]], update accordingly:
   * {{{
   *   Index ~ (CharsWhileIn("0-9_") ~ ("." ~ CharsWhileIn("0-9_")).? ~
   *     ("e" ~ "-".? ~ CharsWhileIn("0-9")).?).! ~
   *     CharsWhileIn(" ", 0).! ~ token(Keyword.alph).?.!
   * }}}
   */
  private def numberOrHex[Unknown: P]: P[Unit] =
    P {
      CharIn("\\+", "\\-").? ~
        CharsWhileIn("0-9_") ~
        ("." ~ CharsWhileIn("0-9_")).? ~
        (!Token.AlphLowercase.lexeme ~ (StringIn("e-", "E-") | CharIn("0-9a-zA-Z_"))).rep
    }

  /**
   * Parses the space and the unit `alph`.
   *
   * {{{
   *   1234.0 alph
   *         ↑___↑
   * }}}
   */
  private def unitAlph[Unknown: P]: P[SoftAST.UnitAlph] =
    P {
      Index ~
        SpaceParser.parseOrFail.? ~
        TokenParser.parseOrFail(Token.AlphLowercase) ~
        Index
    } map {
      case (from, space, unit, to) =>
        SoftAST.UnitAlph(
          index = range(from, to),
          space = space,
          unit = unit
        )
    }

}
