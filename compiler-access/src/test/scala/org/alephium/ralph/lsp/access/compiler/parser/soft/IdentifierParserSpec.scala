// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.Token
import org.alephium.ralph.lsp.access.compiler.parser.soft.TestParser._
import org.alephium.ralph.lsp.access.util.TestFastParse._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class IdentifierParserSpec extends AnyWordSpec with Matchers {

  "disallow reserved tokens to be used as identifier" when {
    "tail has space" in {
      Token.reserved foreach {
        reserved =>
          assertIsFastParseError {
            parseIdentifier(s"${reserved.lexeme} ")
          }
      }
    }

    "tail is end-of-file" in {
      Token.reserved foreach {
        reserved =>
          assertIsFastParseError {
            parseIdentifier(reserved.lexeme)
          }
      }
    }
  }

  "disallow numbers to be identifier" in {
    Array(
      "1",
      "1.0",
      "0.0",
      "0u",
      "0i",
      "1f",
      "1d"
    ) foreach {
      number =>
        assertIsFastParseError {
          parseIdentifier(number)
        }
    }
  }

}
