// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import org.alephium.ralph.lsp.access.compiler.parser.soft.TestParser._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.TestSoftAST._
import org.alephium.ralph.lsp.access.util.TestFastParse._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.OptionValues._

class AlphParserSpec extends AnyWordSpec with Matchers {

  "fail" when {
    "prefix is alph" in {
      assertIsFastParseError {
        parseAlph("alphing")
      }
    }

    "prefix is ALPH" in {
      assertIsFastParseError {
        parseAlph("ALPHing")
      }
    }
  }

  "ALPH" in {
    parseAlph("ALPH").token shouldBe AlphUppercase(">>ALPH<<")
  }

  "alph" in {
    parseAlph("alph").token shouldBe AlphLowercase(">>alph<<")
  }

  "boolean with comments" in {
    Seq("alph", "ALPH") foreach {
      bool =>
        val result =
          parseAlph {
            s"""// comment
               |$bool""".stripMargin
          }.token

        // has comment
        result.documentation.value.comments should have size 1
        result.documentation.value.comments.head.text.value.text shouldBe "comment"

        result.code.text shouldBe bool
    }
  }

}
