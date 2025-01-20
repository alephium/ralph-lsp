package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.range
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}

private case object AssignmentParser {

  def parseOrFail[Unknown: P]: P[SoftAST.Assignment] =
    P {
      Index ~
        leftExpression ~
        SpaceParser.parseOrFail.? ~
        TokenParser.parseOrFail(Token.Equal) ~
        SpaceParser.parseOrFail.? ~
        ExpressionParser.parse ~
        Index
    } map {
      case (from, identifier, postIdentifierSpace, equalToken, postEqualSpace, expression, to) =>
        SoftAST.Assignment(
          index = range(from, to),
          expressionLeft = identifier,
          postIdentifierSpace = postIdentifierSpace,
          equalToken = equalToken,
          postEqualSpace = postEqualSpace,
          expressionRight = expression
        )
    }

  private def leftExpression[Unknown: P]: P[SoftAST.ExpressionAST] =
    P {
      InfixCallParser.parseOrFail |
        MethodCallParser.parseOrFail |
        BlockParser.clause(required = false) |
        VariableDeclarationParser.parseOrFail |
        MutableBindingParser.parseOrFail |
        ReferenceCallParser.parseOrFail |
        AnnotationParser.parseOrFail |
        ParameterParser.parseOrFail |
        NumberParser.parseOrFail |
        BooleanParser.parseOrFail |
        BStringParser.parseOrFail |
        IdentifierParser.parseOrFail
    }

}
