package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.range
import org.alephium.ralph.lsp.access.compiler.parser.soft.CommonParser._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST

private object TemplateParser {

  def parse[Unknown: P] =
    P(Index ~ templateType ~ space ~ identifier ~ spaceOrFail.? ~ ParameterParser.parse ~ spaceOrFail.? ~ BlockParser.clause(mandatory = true) ~ Index) map {
      case (from, templateType, preIdentifierSpace, identifier, preParamSpace, params, postParamSpace, block, to) =>
        SoftAST.Template(
          index = range(from, to),
          templateType = templateType,
          preIdentifierSpace = preIdentifierSpace,
          identifier = identifier,
          preParamSpace = preParamSpace,
          params = params,
          postParamSpace = postParamSpace,
          block = block
        )
    }

  private def templateType[Unknown: P]: P[SoftAST.TemplateToken] =
    P(TokenParser.Contract | TokenParser.TxScript)

}
