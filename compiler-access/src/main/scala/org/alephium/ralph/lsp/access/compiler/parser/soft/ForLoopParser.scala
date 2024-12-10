package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.range
import org.alephium.ralph.lsp.access.compiler.parser.soft.CommonParser._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST

private object ForLoopParser {

  def parseOrFail[Unknown: P]: P[SoftAST.ForStatement] =
    P {
      Index ~
        TokenParser.ForOrFail ~
        spaceOrFail.? ~
        TokenParser.OpenParen ~
        spaceOrFail.? ~
        ExpressionParser.parse ~
        spaceOrFail.? ~
        TokenParser.Semicolon ~
        spaceOrFail.? ~
        ExpressionParser.parse ~
        spaceOrFail.? ~
        TokenParser.Semicolon ~
        spaceOrFail.? ~
        ExpressionParser.parse ~
        spaceOrFail.? ~
        TokenParser.CloseParen ~
        spaceOrFail.? ~
        BlockParser.clause(required = true) ~
        Index
    } map {
      case (from,
            forToken,
            postForSpace,
            openParen,
            postOpenParenSpace,
            expression1,
            postExpression1Space,
            postExpression1Semicolon,
            postExpression1SemicolonSpace,
            expression2,
            postExpression2Space,
            postExpression2Semicolon,
            postExpression2SemicolonSpace,
            expression3,
            postExpression3Space,
            closeParen,
            postCloseParenSpace,
            block,
            to
          ) =>
        SoftAST.ForStatement(
          index = range(from, to),
          forToken = forToken,
          postForSpace = postForSpace,
          openParen = openParen,
          postOpenParenSpace = postOpenParenSpace,
          expression1 = expression1,
          postExpression1Space = postExpression1Space,
          postExpression1Semicolon = postExpression1Semicolon,
          postExpression1SemicolonSpace = postExpression1SemicolonSpace,
          expression2 = expression2,
          postExpression2Space = postExpression2Space,
          postExpression2Semicolon = postExpression2Semicolon,
          postExpression2SemicolonSpace = postExpression2SemicolonSpace,
          expression3 = expression3,
          postExpression3Space = postExpression3Space,
          closeParen = closeParen,
          postCloseParenSpace = postCloseParenSpace,
          block = block
        )
    }

}
