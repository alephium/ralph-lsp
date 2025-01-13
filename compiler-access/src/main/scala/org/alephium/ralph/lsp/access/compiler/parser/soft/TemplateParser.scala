package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.range
import org.alephium.ralph.lsp.access.compiler.parser.soft.CommonParser._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}

private object TemplateParser {

  def parseOrFail[Unknown: P]: P[SoftAST.Template] =
    P {
      Index ~
        (TokenParser.ContractOrFail | TokenParser.TxScriptOrFail) ~
        space ~
        identifier ~
        spaceOrFail.? ~
        ParameterParser.parseOrFail.? ~
        spaceOrFail.? ~
        inheritance.rep ~
        BlockParser.clause(required = true) ~
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
  private def inheritance[Unknown: P]: P[SoftAST.TemplateInheritance] =
    P {
      Index ~
        (TokenParser.parseOrFail(Token.Implements) | TokenParser.parseOrFail(Token.Extends)) ~
        space ~
        (ReferenceCallParser.parseOrFail | identifier) ~
        spaceOrFail.? ~
        Index
    } map {
      case (from, token, preConstructorCallSpace, constructorCall, postConstructorCallSpace, to) =>
        SoftAST.TemplateInheritance(
          index = range(from, to),
          inheritanceType = token,
          preConstructorCallSpace = preConstructorCallSpace,
          reference = constructorCall,
          postConstructorCallSpace = postConstructorCallSpace
        )
    }

}
