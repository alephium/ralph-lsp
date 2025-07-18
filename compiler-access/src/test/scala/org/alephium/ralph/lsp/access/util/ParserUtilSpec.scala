// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.util

import org.alephium.ralph.lsp.access.compiler.parser.soft.TestParser._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.Token
import org.alephium.ralph.lsp.access.util.TestFastParse._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ParserUtilSpec extends AnyWordSpec with Matchers {

  private val allTokens =
    (Token.reserved ++ Token.infix ++ Token.spaces).distinct

  private val tokensWithDuplicatedPrefix =
    allTokens.filter(_.otherReservedTokensWithThisPrefix.nonEmpty)

  "tokens with duplicate prefix" should {
    "match exact tokens" when {
      "+ with +=" in {
        // The token `+=` is not the token `+`
        assertIsFastParseError {
          parseOrTokenCombinator(prefixCheck = true, tokens = Token.Plus)("+=")
        }
      }

      "| with |+|" in {
        // Token `|` is different from token `|+|`
        assertIsFastParseError {
          parseOrTokenCombinator(prefixCheck = true, tokens = Token.Bar)("|*|")
        }
      }

      "& with &&" in {
        // Token `&` is different from token `&&`
        assertIsFastParseError {
          parseOrTokenCombinator(prefixCheck = true, tokens = Token.Ampersand)("&&")
        }
      }

      "all tests" in {
        // Run the above individual tests for all cases
        tokensWithDuplicatedPrefix foreach {
          token =>
            token.otherReservedTokensWithThisPrefix foreach {
              duplicate =>
                assertIsFastParseError {
                  parseOrTokenCombinator(prefixCheck = true, tokens = token)(duplicate.lexeme)
                }
            }

        }
      }
    }
  }

}
