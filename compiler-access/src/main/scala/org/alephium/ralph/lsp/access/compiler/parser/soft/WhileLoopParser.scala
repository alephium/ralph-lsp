package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.range
import org.alephium.ralph.lsp.access.compiler.parser.soft.CommonParser._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}

private object WhileLoopParser {

  def parseOrFail[Unknown: P]: P[SoftAST.WhileStatement] =
    P {
      Index ~
        TokenParser.parseOrFail(Token.While) ~
        spaceOrFail.? ~
        TokenParser.OpenParen ~
        spaceOrFail.? ~
        ExpressionParser.parse ~
        spaceOrFail.? ~
        TokenParser.CloseParen ~
        spaceOrFail.? ~
        BlockParser.clause(required = true) ~
        Index
    } map {
      case (from, whileToken, postWhileSpace, openParen, postOpenParenSpace, expression, postExpressionSpace, closeParen, postCloseParenSpace, block, to) =>
        SoftAST.WhileStatement(
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
