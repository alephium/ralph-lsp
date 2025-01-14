package org.alephium.ralph.lsp.access.compiler.parser.soft

import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra._
import org.alephium.ralph.lsp.access.compiler.parser.soft.TestParser._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.TestSoftAST._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.Token
import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpec

class TokenParserSpec extends AnyWordSpec {

  "InfixOperatorOrFail" when {
    "ForwardSlash" should {
      "not parse double forward slash as it is reserved for comments" in {
        assertThrows[Exception](
          parseInfixOperatorOrFail("//")
        )
      }
    }

    "all infix operators" should {
      "succeed" in {
        val infixOperators = Token.infix
        infixOperators should not be empty

        infixOperators foreach {
          infix =>
            parseInfixOperatorOrFail(infix.lexeme) shouldBe
              TokenDocumented(
                index = range(0, infix.lexeme.length),
                token = infix
              )
        }
      }
    }
  }

}
