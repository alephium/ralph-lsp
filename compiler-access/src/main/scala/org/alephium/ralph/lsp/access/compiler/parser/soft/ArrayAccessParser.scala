// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.range
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}

object ArrayAccessParser {

  /**
   * Note: The following case could result in an unintended parsed AST, but we are strictly following how ralphc parses it.
   *
   * Imagine the following case:
   * {{{
   *   let array = [0, 1, 2]
   *   let copy = array
   *   [1] // This could be an array index-access, or it could also be a new array with a single element?
   * }}}
   *
   * But since ralphc errors when accessing arrays (`[1]`) without usage, we will also parse it similarly.
   *  - In Scala: Index access must be on the same line.
   *  - In Java: Index access can be on a new line.
   */
  private case class Access(
      preOpenBracketSpace: Option[SoftAST.Space],
      openBracket: SoftAST.TokenDocumented[Token.OpenBracket.type],
      preAccessIndex: Option[SoftAST.Space],
      accessIndex: SoftAST.ExpressionAST,
      preCloseBracketSpace: Option[SoftAST.Space],
      unresolved: Option[SoftAST.Unresolved],
      closeBracket: SoftAST.TokenDocExpectedAST[Token.BlockBracket.type],
      to: Int) {

    def on(
        array: SoftAST.ExpressionAST,
        from: Int): SoftAST.ArrayAccess =
      SoftAST.ArrayAccess(
        index = range(from, to),
        identifier = array,
        preOpenBracketSpace = preOpenBracketSpace,
        openBracket = openBracket,
        preAccessIndex = preAccessIndex,
        accessIndex = accessIndex,
        preCloseBracketSpace = preCloseBracketSpace,
        unresolved = unresolved,
        closeBracket = closeBracket
      )

  }

  def parseOrFail[Unknown: P]: P[SoftAST.ArrayAccess] =
    P(Index ~ IdentifierParser.parseOrFail ~ access.rep(1)) flatMap {
      case (from, identifier, head :: tail) =>
        Pass(tail.foldLeft(head.on(identifier, from)) {
          case (array, next) => next.on(array, from)
        })

      case (_, _, accesses) =>
        Fail(s"Expected at least one array access. Actual: ${accesses.size}")
    }

  private def access[Unknown: P]: P[Access] =
    P {
      SpaceParser.parseOrFail.? ~
        TokenParser.parseOrFail(Token.OpenBracket) ~
        SpaceParser.parseOrFail.? ~
        ExpressionParser.parseSubset(expression) ~
        SpaceParser.parseOrFail.? ~
        UnresolvedParser.parseOrFail(Token.BlockBracket).? ~
        TokenParser.parse(Token.BlockBracket) ~
        Index
    } map {
      case (preOpenBracketSpace, openBracket, preAccessIndex, accessIndex, preCloseBracketSpace, unresolved, closeBracket, to) =>
        Access(preOpenBracketSpace, openBracket, preAccessIndex, accessIndex, preCloseBracketSpace, unresolved, closeBracket, to)
    }

  private def expression[Unknown: P]: P[SoftAST.ExpressionAST] =
    P {
      InfixCallParser.parseOrFail |
        MethodCallParser.parseOrFail |
        ArrayAccessParser.parseOrFail |
        ReferenceCallParser.parseOrFail |
        ByteVecParser.parseOrFail |
        NumberParser.parseOrFail |
        IdentifierParser.parseOrFail
    }

}
