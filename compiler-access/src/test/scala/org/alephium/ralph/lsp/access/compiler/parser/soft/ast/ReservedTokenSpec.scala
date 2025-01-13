package org.alephium.ralph.lsp.access.compiler.parser.soft.ast

import fastparse.Parsed
import org.alephium.ralph.lsp.access.compiler.parser.soft.TestParser._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.EitherValues._

import scala.util.Random

class ReservedTokenSpec extends AnyWordSpec with Matchers {

  "succeed" when {
    "tokens are reserved" in {
      val tokens =
        Random.shuffle(Token.reserved.toList)

      tokens should not be empty

      tokens foreach {
        token =>
          parseReservedToken()(token.lexeme) shouldBe token
      }
    }
  }

  "fail" when {
    "token is not reserved" in {
      parseReservedTokenOrError()("blah").left.value shouldBe a[Parsed.Failure]
    }

    "a reserved token is removed" in {
      // First parse without removing Hash.
      parseReservedTokenOrError()(Token.Hash.lexeme).value shouldBe Token.Hash
      // Then parse removing Hash. This should error because Hash is no longer a reserved token.
      parseReservedTokenOrError(remove = Token.Hash)(Token.Hash.lexeme).left.value shouldBe a[Parsed.Failure]
    }
  }

}
