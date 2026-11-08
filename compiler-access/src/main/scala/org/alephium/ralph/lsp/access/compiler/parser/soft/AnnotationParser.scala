// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.{point, range}
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
        annotationIdentifier ~
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

  // Address-shaped expressions share the annotation syntax in the soft AST. Their names can
  // begin with a digit and include a numeric group suffix; ordinary identifiers remain unchanged.
  private def annotationIdentifier[Unknown: P]: P[SoftAST.IdentifierAST] =
    P(Index ~ annotationIdentifierOrFail.?) map {
      case (_, Some(identifier)) => identifier
      case (from, None)          => SoftAST.IdentifierExpected(point(from))
    }

  private def annotationIdentifierOrFail[Unknown: P]: P[SoftAST.Identifier] =
    P {
      Index ~
        CommentParser.parseOrFail.? ~
        !(ReservedTokenParser.parseOrFail ~ TokenParser.isBoundary()) ~
        CodeParser.parseOrFail(annotationName.!) ~
        Index
    } map {
      case (from, documentation, code, to) =>
        SoftAST.Identifier(range(from, to), documentation, code)
    }

  private def annotationName[Unknown: P]: P[Unit] =
    P(CharIn("a-zA-Z0-9_") ~ CharIn("a-zA-Z0-9_!").rep ~ (":" ~ CharIn("0-9").rep(1)).?)

}
