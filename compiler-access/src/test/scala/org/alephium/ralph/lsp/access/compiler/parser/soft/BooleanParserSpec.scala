// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import org.alephium.ralph.lsp.access.compiler.parser.soft.TestParser._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.TestSoftAST._
import org.alephium.ralph.lsp.access.util.TestFastParse._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.OptionValues._

class BooleanParserSpec extends AnyWordSpec with Matchers {

  "fail" when {
    "prefix is boolean" in {
      assertIsFastParseError {
        parseBoolean("truely")
      }
    }

    "prefix is false" in {
      assertIsFastParseError {
        parseBoolean("falsely")
      }
    }
  }

  "true" in {
    parseBoolean("true").token shouldBe True(">>true<<")
  }

  "false" in {
    parseBoolean("false").token shouldBe False(">>false<<")
  }

  "boolean with comments" in {
    Seq("true", "false") foreach {
      bool =>
        val result =
          parseBoolean {
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
