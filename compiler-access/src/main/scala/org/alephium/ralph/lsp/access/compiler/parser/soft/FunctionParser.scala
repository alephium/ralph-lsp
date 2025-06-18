// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.{point, range}
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}

private object FunctionParser {

  /**
   * Parses a function.
   *
   * @return A parsed [[SoftAST.Function]] containing all function information,
   *         such as its documentation, name, parameters, return type and block expressions.
   */
  def parseOrFail[Unknown: P]: P[SoftAST.Function] =
    P {
      Index ~
        AnnotationParser.parseOrFail.rep ~
        AccessModifierParser.parseOrFail.? ~
        TokenParser.parseOrFail(Token.Fn) ~
        TokenParser.isBoundary() ~
        SpaceParser.parseOrFail.? ~
        signature ~
        SpaceParser.parseOrFail.? ~
        BlockParser.parseOrFail.? ~
        Index
    } map {
      case (from, annotation, accessModifier, fnDeceleration, headSpace, signature, tailSpace, block, to) =>
        SoftAST.Function(
          index = range(from, to),
          annotations = annotation,
          accessModifier = accessModifier,
          fn = fnDeceleration,
          preSignatureSpace = headSpace,
          signature = signature,
          postSignatureSpace = tailSpace,
          block = block
        )
    }

  /**
   * Parses a function's signature.
   *
   * @return A parsed [[SoftAST.FunctionSignature]] containing the details of the function signature,
   *         such as its name, parameters and return type.
   */
  private def signature[Unknown: P]: P[SoftAST.FunctionSignature] =
    P {
      Index ~
        IdentifierParser.parse ~
        SpaceParser.parseOrFail.? ~
        TupleParser.parse ~
        SpaceParser.parseOrFail.? ~
        returnSignature ~
        Index
    } map {
      case (from, fnName, headSpace, params, tailSpace, returns, to) =>
        SoftAST.FunctionSignature(
          index = range(from, to),
          fnName = fnName,
          preParamSpace = headSpace,
          params = params,
          postParamSpace = tailSpace,
          returned = returns
        )
    }

  /**
   * Parses a function's return type.
   *
   * @return The parsed [[SoftAST.FunctionReturnAST]], either containing the return
   *         type or an error indicating that the return type is expected.
   */
  private def returnSignature[Unknown: P]: P[SoftAST.FunctionReturnAST] =
    P {
      Index ~
        (TokenParser.parse(Token.ForwardArrow) ~
          SpaceParser.parseOrFail.? ~
          returnExpression).? ~
        Index
    } map {
      case (from, Some((forwardArrow, space, tpe)), to) =>
        SoftAST.FunctionReturn(
          index = range(from, to),
          forwardArrow = forwardArrow,
          space = space,
          tpe = tpe
        )

      case (from, None, to) =>
        SoftAST.FunctionReturnExpected(range(from, to))
    }

  private def returnExpression[Unknown: P]: P[SoftAST.ExpressionAST] =
    P(Index ~ returnExpressionParserOrFail.?) map {
      case (_, Some(expression)) =>
        expression

      case (from, None) =>
        SoftAST.ExpressionExpected(point(from))
    }

  private def returnExpressionParserOrFail[Unknown: P]: P[SoftAST.ExpressionAST] =
    P {
      IfElseParser.parseOrFail |
        ElseParser.parseOrFail |
        TupleParser.parseOrFail |
        NumberParser.parseOrFail |
        IdentifierParser.parseOrFail
    }

}
