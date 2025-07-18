// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

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
  }

}
