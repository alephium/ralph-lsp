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
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.{point, range}
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}
import CommonParser._

private object TokenParser {

  def Colon[Unknown: P]: P[SoftAST.ColonAST] =
    P(Index ~ ColonOrFail.?) map {
      case (_, Some(colon)) =>
        colon

      case (from, None) =>
        SoftAST.ColonExpected(point(from))
    }

  def ColonOrFail[Unknown: P]: P[SoftAST.ColonAST] =
    P(Index ~ CommentParser.parseOrFail.? ~ toCodeOrFail(Token.Colon.lexeme.!) ~ Index) map {
      case (from, documentation, text, to) =>
        SoftAST.Colon(
          index = range(from, to),
          documentation = documentation,
          code = text
        )
    }

  def Semicolon[Unknown: P]: P[SoftAST.SemicolonAST] =
    P(Index ~ SemicolonOrFail.?) map {
      case (_, Some(semicolon)) =>
        semicolon

      case (from, None) =>
        SoftAST.SemicolonExpected(point(from))
    }

  def SemicolonOrFail[Unknown: P]: P[SoftAST.SemicolonAST] =
    P(Index ~ CommentParser.parseOrFail.? ~ toCodeOrFail(Token.Semicolon.lexeme.!) ~ Index) map {
      case (from, documentation, text, to) =>
        SoftAST.Semicolon(
          index = range(from, to),
          documentation = documentation,
          code = text
        )
    }

  def OpenParen[Unknown: P](required: Boolean): P[SoftAST.OpenParenAST] =
    if (required)
      OpenParen
    else
      OpenParenOrFail

  def OpenParen[Unknown: P]: P[SoftAST.OpenParenAST] =
    P(Index ~ OpenParenOrFail.?) map {
      case (_, Some(openParen)) =>
        openParen

      case (from, None) =>
        SoftAST.OpenParenExpected(point(from))
    }

  def OpenParenOrFail[Unknown: P]: P[SoftAST.OpenParen] =
    P(Index ~ CommentParser.parseOrFail.? ~ toCodeOrFail(Token.OpenParen.lexeme.!) ~ Index) map {
      case (from, documentation, text, to) =>
        SoftAST.OpenParen(
          index = range(from, to),
          documentation = documentation,
          code = text
        )
    }

  def CloseParen[Unknown: P]: P[SoftAST.CloseParenAST] =
    P(Index ~ CloseParenOrFail.?) map {
      case (_, Some(closeParen)) =>
        closeParen

      case (from, None) =>
        SoftAST.CloseParenExpected(point(from))
    }

  def CloseParenOrFail[Unknown: P]: P[SoftAST.CloseParenAST] =
    P(Index ~ CommentParser.parseOrFail.? ~ toCodeOrFail(Token.CloseParen.lexeme.!) ~ Index) map {
      case (from, documentation, text, to) =>
        SoftAST.CloseParen(
          index = range(from, to),
          documentation = documentation,
          code = text
        )
    }

  def OpenCurly[Unknown: P](required: Boolean): P[SoftAST.OpenCurlyAST] =
    if (required)
      OpenCurly
    else
      OpenCurlyOrFail

  def OpenCurly[Unknown: P]: P[SoftAST.OpenCurlyAST] =
    P(Index ~ OpenCurlyOrFail.?) map {
      case (_, Some(openCurly)) =>
        openCurly

      case (from, None) =>
        SoftAST.OpenCurlyExpected(point(from))
    }

  def OpenCurlyOrFail[Unknown: P]: P[SoftAST.OpenCurly] =
    P(Index ~ CommentParser.parseOrFail.? ~ toCodeOrFail(Token.OpenCurly.lexeme.!) ~ Index) map {
      case (from, documentation, text, to) =>
        SoftAST.OpenCurly(
          index = range(from, to),
          documentation = documentation,
          code = text
        )
    }

  def CloseCurly[Unknown: P]: P[SoftAST.CloseCurlyAST] =
    P(Index ~ CloseCurlyOrFail.?) map {
      case (_, Some(closeCurly)) =>
        closeCurly

      case (from, None) =>
        SoftAST.CloseCurlyExpected(point(from))
    }

  def CloseCurlyOrFail[Unknown: P]: P[SoftAST.CloseCurlyAST] =
    P(Index ~ CommentParser.parseOrFail.? ~ toCodeOrFail(Token.CloseCurly.lexeme.!) ~ Index) map {
      case (from, documentation, text, to) =>
        SoftAST.CloseCurly(
          index = range(from, to),
          documentation = documentation,
          code = text
        )
    }

  def CommaOrFail[Unknown: P]: P[SoftAST.Comma] =
    P(Index ~ CommentParser.parseOrFail.? ~ toCodeOrFail(Token.Comma.lexeme.!) ~ Index) map {
      case (from, documentation, text, to) =>
        SoftAST.Comma(
          index = range(from, to),
          documentation = documentation,
          code = text
        )
    }

  def ForwardArrow[Unknown: P]: P[SoftAST.ForwardArrowAST] =
    P(Index ~ ForwardArrowOrFail.?) map {
      case (_, Some(forwardArrow)) =>
        forwardArrow

      case (from, None) =>
        SoftAST.ForwardArrowExpected(point(from))
    }

  def ForwardArrowOrFail[Unknown: P]: P[SoftAST.ForwardArrowAST] =
    P(Index ~ CommentParser.parseOrFail.? ~ toCodeOrFail(Token.ForwardArrow.lexeme.!) ~ Index) map {
      case (from, documentation, text, to) =>
        SoftAST.ForwardArrow(
          index = range(from, to),
          documentation = documentation,
          code = text
        )
    }

  def DoubleForwardSlashOrFail[Unknown: P]: P[SoftAST.DoubleForwardSlash] =
    P(toCodeOrFail(Token.DoubleForwardSlash.lexeme.!)) map {
      text =>
        SoftAST.DoubleForwardSlash(text)
    }

  def ContractOrFail[Unknown: P]: P[SoftAST.Contract] =
    P(Index ~ CommentParser.parseOrFail.? ~ toCodeOrFail(Token.Contract.lexeme.!) ~ Index) map {
      case (from, documentation, text, to) =>
        SoftAST.Contract(
          index = range(from, to),
          documentation = documentation,
          code = text
        )
    }

  def TxScriptOrFail[Unknown: P]: P[SoftAST.TxScript] =
    P(Index ~ CommentParser.parseOrFail.? ~ toCodeOrFail(Token.TxScript.lexeme.!) ~ Index) map {
      case (from, documentation, text, to) =>
        SoftAST.TxScript(
          index = range(from, to),
          documentation = documentation,
          code = text
        )
    }

  def StructOrFail[Unknown: P]: P[SoftAST.Struct] =
    P(Index ~ CommentParser.parseOrFail.? ~ toCodeOrFail(Token.Struct.lexeme.!) ~ Index) map {
      case (from, documentation, text, to) =>
        SoftAST.Struct(
          index = range(from, to),
          documentation = documentation,
          code = text
        )
    }

  def EnumOrFail[Unknown: P]: P[SoftAST.Enum] =
    P(Index ~ CommentParser.parseOrFail.? ~ toCodeOrFail(Token.Enum.lexeme.!) ~ Index) map {
      case (from, documentation, text, to) =>
        SoftAST.Enum(
          index = range(from, to),
          documentation = documentation,
          code = text
        )
    }

  def EventOrFail[Unknown: P]: P[SoftAST.Event] =
    P(Index ~ CommentParser.parseOrFail.? ~ toCodeOrFail(Token.Event.lexeme.!) ~ Index) map {
      case (from, documentation, text, to) =>
        SoftAST.Event(
          index = range(from, to),
          documentation = documentation,
          code = text
        )
    }

  def FnOrFail[Unknown: P]: P[SoftAST.Fn] =
    P(Index ~ CommentParser.parseOrFail.? ~ toCodeOrFail(Token.Fn.lexeme.!) ~ Index) map {
      case (from, documentation, text, to) =>
        SoftAST.Fn(
          index = range(from, to),
          documentation = documentation,
          code = text
        )
    }

  def ImplementsOrFail[Unknown: P]: P[SoftAST.Implements] =
    P(Index ~ CommentParser.parseOrFail.? ~ toCodeOrFail(Token.Implements.lexeme.!) ~ Index) map {
      case (from, documentation, text, to) =>
        SoftAST.Implements(
          index = range(from, to),
          documentation = documentation,
          code = text
        )
    }

  def ExtendsOrFail[Unknown: P]: P[SoftAST.Extends] =
    P(Index ~ CommentParser.parseOrFail.? ~ toCodeOrFail(Token.Extends.lexeme.!) ~ Index) map {
      case (from, documentation, text, to) =>
        SoftAST.Extends(
          index = range(from, to),
          documentation = documentation,
          code = text
        )
    }

  def DotOrFail[Unknown: P]: P[SoftAST.Dot] =
    P(Index ~ CommentParser.parseOrFail.? ~ toCodeOrFail(Token.Dot.lexeme.!) ~ Index) map {
      case (from, documentation, text, to) =>
        SoftAST.Dot(
          index = range(from, to),
          documentation = documentation,
          code = text
        )
    }

  def ReturnOrFail[Unknown: P]: P[SoftAST.Return] =
    P(Index ~ CommentParser.parseOrFail.? ~ toCodeOrFail(Token.Return.lexeme.!) ~ Index) map {
      case (from, documentation, text, to) =>
        SoftAST.Return(
          index = range(from, to),
          documentation = documentation,
          code = text
        )
    }

  def ForOrFail[Unknown: P]: P[SoftAST.For] =
    P(Index ~ CommentParser.parseOrFail.? ~ toCodeOrFail(Token.For.lexeme.!) ~ Index) map {
      case (from, documentation, text, to) =>
        SoftAST.For(
          index = range(from, to),
          documentation = documentation,
          code = text
        )
    }

  def WhileOrFail[Unknown: P]: P[SoftAST.While] =
    P(Index ~ CommentParser.parseOrFail.? ~ toCodeOrFail(Token.While.lexeme.!) ~ Index) map {
      case (from, documentation, text, to) =>
        SoftAST.While(
          index = range(from, to),
          documentation = documentation,
          code = text
        )
    }

  def EqualOrFail[Unknown: P]: P[SoftAST.Equal] =
    P(Index ~ CommentParser.parseOrFail.? ~ toCodeOrFail(Token.Equal.lexeme.!) ~ Index) map {
      case (from, documentation, text, to) =>
        SoftAST.Equal(
          index = range(from, to),
          documentation = documentation,
          code = text
        )
    }

  def LetOrFail[Unknown: P]: P[SoftAST.Let] =
    P(Index ~ CommentParser.parseOrFail.? ~ toCodeOrFail(Token.Let.lexeme.!) ~ Index) map {
      case (from, documentation, text, to) =>
        SoftAST.Let(
          index = range(from, to),
          documentation = documentation,
          code = text
        )
    }

  def MutOrFail[Unknown: P]: P[SoftAST.Mut] =
    P(Index ~ CommentParser.parseOrFail.? ~ toCodeOrFail(Token.Mut.lexeme.!) ~ Index) map {
      case (from, documentation, text, to) =>
        SoftAST.Mut(
          index = range(from, to),
          documentation = documentation,
          code = text
        )
    }

  def PubOrFail[Unknown: P]: P[SoftAST.Pub] =
    P(Index ~ CommentParser.parseOrFail.? ~ toCodeOrFail(Token.Pub.lexeme.!) ~ Index) map {
      case (from, documentation, text, to) =>
        SoftAST.Pub(
          index = range(from, to),
          documentation = documentation,
          code = text
        )
    }

  def AtOrFail[Unknown: P]: P[SoftAST.At] =
    P(Index ~ CommentParser.parseOrFail.? ~ toCodeOrFail(Token.At.lexeme.!) ~ Index) map {
      case (from, documentation, text, to) =>
        SoftAST.At(
          index = range(from, to),
          documentation = documentation,
          code = text
        )
    }

  /**
   * Parses all reserved tokens defined in [[Token.reserved]] and returns the first match.
   */
  def Reserved[Unknown: P]: P[Token.Reserved] = {
    val it =
      Token.reserved.iterator

    val head =
      P(it.next().lexeme) map {
        _ =>
          Token.reserved.head
      }

    it.foldLeft(head) {
      case (parser, keyword) =>
        def nextParser =
          P(keyword.lexeme) map {
            _ =>
              keyword
          }

        parser | nextParser
    }
  }

  def OperatorOrFail[Unknown: P]: P[SoftAST.Operator] =
    P {
      buildOperatorParser(Token.Or) |
        buildOperatorParser(Token.And) |
        buildOperatorParser(Token.GreaterThanOrEqual) |
        buildOperatorParser(Token.LessThanOrEqual) |
        buildOperatorParser(Token.PlusEquals) |
        buildOperatorParser(Token.MinusEquals) |
        buildOperatorParser(Token.Equality) |
        buildOperatorParser(Token.Asterisk) |
        buildOperatorParser(Token.GreaterThan) |
        buildOperatorParser(Token.LessThan) |
        buildOperatorParser(Token.NotEqual) |
        buildOperatorParser(Token.Bar) |
        buildOperatorParser(Token.Ampersand) |
        buildOperatorParser(Token.Caret) |
        buildOperatorParser(Token.Percent) |
        buildOperatorParser(Token.Minus) |
        buildOperatorParser(Token.Plus) |
        // Forward-slash followed by another forward-slash is not an Operator. `//` is reserved for comment prefix.
        P(buildOperatorParser(Token.ForwardSlash) ~ !Token.ForwardSlash.lexeme)
    }

  private def buildOperatorParser[Unknown: P](operator: Token.Operator): P[SoftAST.Operator] =
    P(Index ~ CommentParser.parseOrFail.? ~ toCodeOrFail(operator.lexeme.!) ~ Index) map {
      case (from, documentation, text, to) =>
        SoftAST.Operator(
          index = range(from, to),
          documentation = documentation,
          code = text
        )
    }

}
