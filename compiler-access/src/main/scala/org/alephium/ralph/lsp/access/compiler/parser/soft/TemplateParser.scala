package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.range
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}

private object TemplateParser {

  def parseOrFail[Unknown: P]: P[SoftAST.Template] =
    P {
      Index ~
        (TokenParser.parseOrFail(Token.Contract) | TokenParser.parseOrFail(Token.TxScript) | TokenParser.parseOrFail(Token.AssetScript)) ~
        TokenParser.isBoundary() ~
        SpaceParser.parseOrFail.? ~
        IdentifierParser.parse ~
        SpaceParser.parseOrFail.? ~
        ParameterParser.parseOrFail.? ~
        SpaceParser.parseOrFail.? ~
        inheritance.rep ~
        BlockParser.parse ~
        Index
    } map {
      case (from, templateType, preIdentifierSpace, identifier, preParamSpace, params, postParamSpace, inheritance, block, to) =>
        SoftAST.Template(
          index = range(from, to),
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

  /** Syntax: `implements or extends contract(arg1, arg2 ...)` */
  private def inheritance[Unknown: P]: P[SoftAST.Inheritance] =
    P {
      Index ~
        (TokenParser.parseOrFail(Token.Implements) | TokenParser.parseOrFail(Token.Extends)) ~
        TokenParser.isBoundary() ~
        SpaceParser.parseOrFail.? ~
        (ReferenceCallParser.parseOrFail | IdentifierParser.parse) ~
        SpaceParser.parseOrFail.? ~
        Index
    } map {
      case (from, token, postInheritanceTypeSpace, constructorCall, postConstructorCallSpace, to) =>
        SoftAST.Inheritance(
          index = range(from, to),
          inheritanceType = token,
          postInheritanceTypeSpace = postInheritanceTypeSpace,
          reference = constructorCall,
          postConstructorCallSpace = postConstructorCallSpace
        )
    }

}
