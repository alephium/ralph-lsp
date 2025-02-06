package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.range
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}

private object WhileParser {

  def parseOrFail[Unknown: P]: P[SoftAST.While] =
    P {
      Index ~
        TokenParser.parseOrFail(Token.While) ~
        SpaceParser.parseOrFail.? ~
        TokenParser.parse(Token.OpenParen) ~
        SpaceParser.parseOrFail.? ~
        ExpressionParser.parse ~
        SpaceParser.parseOrFail.? ~
        TokenParser.parse(Token.CloseParen) ~
        SpaceParser.parseOrFail.? ~
        BlockParser.parse ~
        Index
    } map {
      case (from, whileToken, postWhileSpace, openParen, postOpenParenSpace, expression, postExpressionSpace, closeParen, postCloseParenSpace, block, to) =>
        SoftAST.While(
          index = range(from, to),
          whileToken = whileToken,
          postWhileSpace = postWhileSpace,
          openParen = openParen,
          postOpenParenSpace = postOpenParenSpace,
          expression = expression,
          postExpressionSpace = postExpressionSpace,
          closeParen = closeParen,
          postCloseParenSpace = postCloseParenSpace,
          block = block
        )
    }

}
