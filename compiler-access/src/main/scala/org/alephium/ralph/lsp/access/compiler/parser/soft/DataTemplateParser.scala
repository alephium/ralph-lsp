package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.range
import org.alephium.ralph.lsp.access.compiler.parser.soft.CommonParser._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST

private object DataTemplateParser {

  def parseOrFail[Unknown: P]: P[SoftAST.DataTemplate] =
    P {
      Index ~
        (TokenParser.StructOrFail | TokenParser.EnumOrFail | TokenParser.EventOrFail) ~
        space ~
        identifier ~
        spaceOrFail.? ~
        ParameterParser.parse ~
        Index
    } map {
      case (from, templateType, preIdentifierSpace, identifier, preParamSpace, params, to) =>
        SoftAST.DataTemplate(
          index = range(from, to),
          dataType = templateType,
          preIdentifierSpace = preIdentifierSpace,
          identifier = identifier,
          preParamSpace = preParamSpace,
          params = params
        )
    }

}
