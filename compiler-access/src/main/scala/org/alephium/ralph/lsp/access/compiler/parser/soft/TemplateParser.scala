// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.range
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}

private object TemplateParser {

  def parseOrFail[Unknown: P]: P[SoftAST.Template] =
    P {
      Index ~
        AnnotationParser.parseOrFail.rep ~
        abstractToken.? ~
        templateType ~
        SpaceParser.parseOrFail.? ~
        IdentifierParser.parse ~
        SpaceParser.parseOrFail.? ~
        TypeAssignmentGroupParser.parseOrFail(Token.OpenParen, Token.CloseParen).? ~
        SpaceParser.parseOrFail.? ~
        InheritanceParser.parseOrFail.rep ~
        BlockParser.parseOrFail.? ~
        Index
    } map {
      case (from, annotations, abstractToken, templateType, preIdentifierSpace, identifier, preParamSpace, params, postParamSpace, inheritance, block, to) =>
        SoftAST.Template(
          index = range(from, to),
          annotations = annotations,
          abstracted = abstractToken,
          templateType = templateType,
          preIdentifierSpace = preIdentifierSpace,
          identifier = identifier,
          preParamSpace = preParamSpace,
          params = params,
          postParamSpace = postParamSpace,
          inheritance = inheritance,
          block = block
        )
    }

  private def templateType[Unknown: P]: P[SoftAST.TokenDocumented[Token.TemplateDefinition]] =
    P(typeDefinitions ~ TokenParser.isBoundary())

  private def typeDefinitions[Unknown: P]: P[SoftAST.TokenDocumented[Token.TemplateDefinition]] =
    P {
      TokenParser.parseOrFailOneOf(
        prefixCheck = false,
        tokens = Iterator(Token.Contract, Token.TxScript, Token.AssetScript, Token.Interface)
      )
    }

  private def abstractToken[Unknown: P]: P[SoftAST.Abstract] =
    P {
      Index ~
        TokenParser.parseOrFail(Token.Abstract) ~
        TokenParser.isBoundary() ~
        SpaceParser.parseOrFail.? ~
        Index
    } map {
      case (from, token, space, to) =>
        SoftAST.Abstract(
          index = range(from, to),
          abstractToken = token,
          postAbstractSpace = space
        )
    }

}
