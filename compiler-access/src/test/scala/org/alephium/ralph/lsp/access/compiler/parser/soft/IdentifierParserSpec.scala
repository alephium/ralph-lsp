package org.alephium.ralph.lsp.access.compiler.parser.soft

import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.Token
import org.alephium.ralph.lsp.access.compiler.parser.soft.TestParser._
import org.alephium.ralph.lsp.access.util.TestFastParse._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class IdentifierParserSpec extends AnyWordSpec with Matchers {

  "disallow reserved tokens to be used as identifier" when {
    val reserved =
      Token.reserved.diff(Seq(Token.Hash)) // Remove hash because `let hash = #` is valid

    "tail has space" in {
      reserved foreach {
        reserved =>
          assertIsFastParseError {
            parseIdentifier(s"${reserved.lexeme} ")
          }
      }
    }

    "tail is end-of-file" in {
      reserved foreach {
        reserved =>
          assertIsFastParseError {
            parseIdentifier(reserved.lexeme)
          }
      }
    }
  }

}
