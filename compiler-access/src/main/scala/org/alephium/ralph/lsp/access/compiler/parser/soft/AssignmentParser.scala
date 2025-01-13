package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.range
import org.alephium.ralph.lsp.access.compiler.parser.soft.CommonParser._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}

private object AssignmentParser {

  def parseOrFail[Unknown: P]: P[SoftAST.Assignment] =
    P {
      Index ~
        AssignmentAccessModifierParser.parseOrFail.rep ~
        IdentifierParser.parseOrFail ~
        spaceOrFail.? ~
        TokenParser.parseOrFail(Token.Equal) ~
        spaceOrFail.? ~
        ExpressionParser.parse ~
        Index
    } map {
      case (from, control, identifier, postIdentifierSpace, equalToken, postEqualSpace, expression, to) =>
        SoftAST.Assignment(
          index = range(from, to),
          modifiers = control,
          identifier = identifier,
          postIdentifierSpace = postIdentifierSpace,
          equalToken = equalToken,
          postEqualSpace = postEqualSpace,
          expression = expression
        )
    }

}
