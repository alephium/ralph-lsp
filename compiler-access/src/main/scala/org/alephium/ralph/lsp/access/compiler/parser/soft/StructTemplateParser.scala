package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.range
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}

private object StructTemplateParser {

  def parseOrFail[Unknown: P]: P[SoftAST.StructTemplate] =
    P {
      Index ~
        TokenParser.parseOrFail(Token.Struct) ~
        SpaceParser.parse ~
        IdentifierParser.parse ~
        SpaceParser.parseOrFail.? ~
        GroupParser.parse(Token.OpenCurly, Token.CloseCurly) ~
        Index
    } map {
      case (from, structToken, preIdentifierSpace, identifier, preParamSpace, params, to) =>
        SoftAST.StructTemplate(
          index = range(from, to),
          structToken = structToken,
          preIdentifierSpace = preIdentifierSpace,
          identifier = identifier,
          preParamSpace = preParamSpace,
          params = params
        )
    }

}
