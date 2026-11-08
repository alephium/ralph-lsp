// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.range
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}

/** Parses Ralph test declarations, their settings, setup/teardown clauses and repeated bodies. */
private object TestDeclarationParser {

  def parseOrFail[Unknown: P]: P[SoftAST.TestDeclaration] =
    P {
      Index ~
        TokenParser.parseOrFail(Token.Test) ~
        TokenParser.isBoundary() ~
        SpaceParser.parseOrFail.? ~
        StringLiteralParser.parseOrFail ~
        (SpaceParser.parseOrFail.? ~ (clause | BlockParser.parseOrFail)).rep ~
        Index
    } map {
      case (from, keyword, preNameSpace, name, parts, to) =>
        SoftAST.TestDeclaration(
          range(from, to),
          keyword,
          preNameSpace,
          name,
          parts.flatMap {
            case (space, part) => space.toSeq :+ part
          }
        )
    }

  private def clauseKeyword[Unknown: P]: P[SoftAST.TokenDocumented[Token.TestClauseKeyword]] =
    P {
      TokenParser.parseOrFailOneOf(prefixCheck = false, tokens = Iterator(Token.With, Token.Before, Token.After, Token.Approve)) ~
        TokenParser.isBoundary() ~
        !(SpaceParser.parseOrFail.? ~ (Token.Equal.lexeme | Token.OpenParen.lexeme))
    }

  private def clause[Unknown: P]: P[SoftAST.TestClause] =
    P(Index ~ clauseKeyword) flatMap {
      case (from, keyword) =>
        val body =
          if (keyword.code.token == Token.Approve)
            P(SpaceParser.parseOrFail.? ~ ExpressionParser.parseSubset(AssetApprovalParser.parseOrFail)).map {
              case (space, approval) => space.toSeq :+ approval
            }
          else
            clausePart.rep
        P(body ~ Index).map {
          case (parts, to) => SoftAST.TestClause(range(from, to), keyword, parts)
        }
    }

  private def clausePart[Unknown: P]: P[SoftAST] =
    P {
      !(clauseKeyword | (TokenParser.parseOrFail(Token.Test) ~ TokenParser.isBoundary())) ~
        !Token.OpenCurly.lexeme ~ !Token.CloseCurly.lexeme ~
        (SpaceParser.parseOrFail |
          ExpressionParser.parseOrFail |
          TokenParser.parseOrFail(Token.Comma) |
          CommentParser.parseOrFail)
    }

}
