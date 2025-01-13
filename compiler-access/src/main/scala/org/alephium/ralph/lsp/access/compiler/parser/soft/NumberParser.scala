package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}

object NumberParser {

  def parseOrFail[Unknown: P]: P[SoftAST.Number] =
    P {
      Index ~
        CommentParser.parseOrFail.? ~
        number ~
        SpaceParser.parseOrFail.? ~
        TokenParser.parseOrFail(Token.AlphLowercase).? ~
        Index
    } map {
      case (from, documentation, digits, postDigitSpace, unit, to) =>
        SoftAST.Number(
          index = range(from, to),
          documentation = documentation,
          number = digits,
          space = postDigitSpace,
          unit = unit
        )
    }

  private def number[Unknown: P]: P[SoftAST.CodeString] =
    P {
      Index ~
        isNumber ~
        (!Token.AlphLowercase.lexeme ~ CharIn("[0-9a-zA-Z]._+\\-")).rep(1).! ~
        Index
    } map {
      case (from, number, to) =>
        SoftAST.CodeString(
          index = range(from, to),
          text = number
        )
    }

  private def isNumber[Unknown: P]: P[Unit] =
    P(&(CharIn("+\\-").? ~ CharIn("[0-9]")))

}
