package org.alephium.ralph.lsp.access.compiler.parser.soft.ast

import fastparse.Parsed
import org.alephium.ralph.lsp.access.compiler.parser.soft.TestParser._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.EitherValues._

import scala.util.Random

class ReservedTokenSpec extends AnyWordSpec with Matchers {

  "parse reserved tokens" in {
    val tokens =
      Random.shuffle(Token.reserved.toList)

    tokens should not be empty

    tokens foreach {
      token =>
        parseReservedToken(token.lexeme) shouldBe token
    }
  }

  "parse reserved without spaces" in {
    parseReservedTokenOrError("blah").left.value shouldBe a[Parsed.Failure]
  }

}
