package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.range
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}

private object TemplateParser {

  def parseOrFail[Unknown: P]: P[SoftAST.Template] =
    P {
      Index ~
        abstractToken.? ~
        templateType ~
        SpaceParser.parseOrFail.? ~
        IdentifierParser.parse ~
        SpaceParser.parseOrFail.? ~
        ParameterParser.parseOrFail.? ~
        SpaceParser.parseOrFail.? ~
        InheritanceParser.parseOrFail.rep ~
        BlockParser.parseOrFail.? ~
        Index
    } map {
      case (from, abstractToken, templateType, preIdentifierSpace, identifier, preParamSpace, params, postParamSpace, inheritance, block, to) =>
        SoftAST.Template(
          index = range(from, to),
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
    P {
      (TokenParser.parseOrFail(Token.Contract) |
        TokenParser.parseOrFail(Token.TxScript) |
        TokenParser.parseOrFail(Token.AssetScript) |
        TokenParser.parseOrFail(Token.Interface)) ~
        TokenParser.isBoundary()
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
