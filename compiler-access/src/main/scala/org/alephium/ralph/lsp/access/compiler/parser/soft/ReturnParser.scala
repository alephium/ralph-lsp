package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.range
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}

private object ReturnParser {

  def parseOrFail[Unknown: P]: P[SoftAST.Return] =
    P {
      Index ~
        TokenParser.parseOrFail(Token.Return) ~
        TokenParser.isBoundary() ~
        SpaceParser.parseOrFail.? ~
        ExpressionParser.parseSubset(expression) ~
        Index
    } map {
      case (from, returnStatement, space, expression, to) =>
        SoftAST.Return(
          index = range(from, to),
          returnToken = returnStatement,
          preExpressionSpace = space,
          rightExpression = expression
        )
    }

  private def expression[Unknown: P]: P[SoftAST.ExpressionAST] =
    P {
      GroupParser.parseOrFail |
        InfixCallParser.parseOrFail |
        MethodCallParser.parseOrFail |
        AnnotationParser.parseOrFail |
        ParameterParser.parseOrFail |
        NumberParser.parseOrFail |
        BooleanParser.parseOrFail |
        BStringParser.parseOrFail |
        StringLiteralParser.parseOrFail |
        IdentifierParser.parseOrFail
    }

}
