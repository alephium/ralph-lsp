package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.range
import org.alephium.ralph.lsp.access.compiler.parser.soft.CommonParser._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST

private object ReturnStatementParser {

  def parseOrFail[Unknown: P]: P[SoftAST.ReturnStatement] =
    P {
      Index ~
        TokenParser.ReturnOrFail ~
        space ~
        ExpressionParser.parse ~
        Index
    } map {
      case (from, returnStatement, space, expression, to) =>
        SoftAST.ReturnStatement(
          index = range(from, to),
          returnToken = returnStatement,
          preExpressionSpace = space,
          rightExpression = expression
        )
    }

}
