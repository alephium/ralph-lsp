package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.range
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}

private object AssignmentParser {

  def parseOrFail[Unknown: P]: P[SoftAST.Assignment] =
    P {
      Index ~
        IdentifierParser.parseOrFail ~
        SpaceParser.parseOrFail.? ~
        TokenParser.parseOrFail(Token.Equal) ~
        SpaceParser.parseOrFail.? ~
        ExpressionParser.parse ~
        Index
    } map {
      case (from, identifier, postIdentifierSpace, equalToken, postEqualSpace, expression, to) =>
        SoftAST.Assignment(
          index = range(from, to),
          identifier = identifier,
          postIdentifierSpace = postIdentifierSpace,
          equalToken = equalToken,
          postEqualSpace = postEqualSpace,
          expression = expression
        )
    }

}
