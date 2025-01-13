package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.range
import org.alephium.ralph.lsp.access.compiler.parser.soft.CommonParser._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}

private object TypeAssignmentParser {

  def parseOrFail[Unknown: P]: P[SoftAST.TypeAssignment] =
    P {
      Index ~
        AssignmentAccessModifierParser.parseOrFail.rep ~
        IdentifierParser.parseOrFail ~
        spaceOrFail.? ~
        TokenParser.parseOrFail(Token.Colon) ~
        spaceOrFail.? ~
        TypeParser.parse ~
        Index
    } map {
      case (from, access, identifier, postIdentifierSpace, equalToken, postEqualSpace, expression, to) =>
        SoftAST.TypeAssignment(
          index = range(from, to),
          modifiers = access,
          name = identifier,
          preColonSpace = postIdentifierSpace,
          colon = equalToken,
          postColonSpace = postEqualSpace,
          tpe = expression
        )
    }

}
