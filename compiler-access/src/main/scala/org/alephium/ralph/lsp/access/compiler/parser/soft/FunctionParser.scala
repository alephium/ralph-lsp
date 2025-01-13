// Copyright 2024 The Alephium Authors
// This file is part of the alephium project.
//
// The library is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// The library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with the library. If not, see http://www.gnu.org/licenses/.

package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.range
import org.alephium.ralph.lsp.access.compiler.parser.soft.CommonParser._
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
        spaceOrFail.? ~
        AccessModifierParser.parseOrFail.? ~
        TokenParser.parseOrFail(Token.Fn) ~
        space ~
        signature ~
        spaceOrFail.? ~
        BlockParser.clause(required = false).? ~
        Index
    } map {
      case (from, annotation, postAnnotationSpace, pub, fnDeceleration, headSpace, signature, tailSpace, block, to) =>
        SoftAST.Function(
          index = range(from, to),
          annotations = annotation,
          postAnnotationSpace = postAnnotationSpace,
          pub = pub,
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
        identifier ~
        spaceOrFail.? ~
        ParameterParser.parse ~
        spaceOrFail.? ~
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
        (TokenParser.parse(Token.ForwardArrow) ~ spaceOrFail.? ~ TypeParser.parse).? ~
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

}
