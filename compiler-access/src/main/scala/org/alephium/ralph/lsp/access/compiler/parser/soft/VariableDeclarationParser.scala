package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.range
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}

private object VariableDeclarationParser {

  /** Syntax: let mut variable = some_expression */
  def parseOrFail[Unknown: P]: P[SoftAST.VariableDeclaration] =
    P {
      Index ~
        TokenParser.parseOrFail(Token.Let) ~
        TokenParser.isBoundary() ~
        SpaceParser.parseOrFail.? ~
        AssignmentParser.parseOrFail ~
        Index
    } map {
      case (from, let, space, assignment, to) =>
        SoftAST.VariableDeclaration(
          index = range(from, to),
          let = let,
          postLetSpace = space,
          assignment = assignment
        )
    }

}
