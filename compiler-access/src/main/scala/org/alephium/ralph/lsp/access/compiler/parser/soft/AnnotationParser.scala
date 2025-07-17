// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.range
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}

private object AnnotationParser {

  /**
   * Parses a single annotation and its trailing space.
   *
   * The configuration within an `Annotation` syntax allows for various expressions [[SoftAST.ExpressionAST]].
   * For example, the following will parse successfully:
   *
   * {{{
   *    @using(a = functionCall(), b = object.getConfig(param), 1 + 1, blah)
   * }}}
   *
   * @return Returns a [[SoftAST.Annotation]] representation the annotation and its documentation.
   */
  def parseOrFail[Unknown: P]: P[SoftAST.Annotation] =
    P {
      Index ~
        TokenParser.parseOrFail(Token.At) ~
        SpaceParser.parseOrFail.? ~
        IdentifierParser.parse ~
        SpaceParser.parseOrFail.? ~
        TupleParser.parseOrFail(assertNonEmpty = true).? ~
        SpaceParser.parseOrFail.? ~
        Index
    } map {
      case (from, at, preIdentifierSpace, identifier, postIdentifierSpace, tuple, postTupleSpace, to) =>
        SoftAST.Annotation(
          index = range(from, to),
          at = at,
          preIdentifierSpace = preIdentifierSpace,
          identifier = identifier,
          postIdentifierSpace = postIdentifierSpace,
          tuple = tuple,
          postTupleSpace = postTupleSpace
        )
    }

}
