package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.range
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST

private object VariableDeclarationParser {

  /** Syntax: let mut variable = some_expression */
  def parseOrFail[Unknown: P]: P[SoftAST.VariableDeclaration] =
    P {
      Index ~
        AssignmentAccessModifierParser.parseOrFail.rep(1) ~
        AssignmentParser.parseOrFail ~
        Index
    } map {
      case (from, modifier, assignment, to) =>
        SoftAST.VariableDeclaration(
          index = range(from, to),
          modifiers = modifier,
          assignment = assignment
        )
    }

}
