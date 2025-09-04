// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import org.alephium.ralph.lsp.access.compiler.parser.soft.TestParser._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.TestSoftAST._
import org.alephium.ralph.lsp.access.util.TestCodeUtil._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class FnDecelerationSpec extends AnyWordSpec with Matchers {

  "report missing function identifier" when {
    "without spaces" in {
      val function =
        parseFunction("fn")

      function.fn shouldBe Fn(">>fn<<")
      function.preSignatureSpace shouldBe None
      function.postSignatureSpace shouldBe empty
      function.block shouldBe empty
    }

    "with head space" in {
      val functions =
        parseSoft(" fn")
          .parts
          .collect {
            case function: SoftAST.Function =>
              function
          }

      functions should have size 1
      val function = functions.head

      function.fn shouldBe Fn(" >>fn<<")
      function.preSignatureSpace shouldBe None
      function.postSignatureSpace shouldBe empty
      function.block shouldBe empty
    }

    "with tail space" in {
      val function =
        parseFunction("fn  ")

      function.fn shouldBe Fn(">>fn<<  ")

      function.preSignatureSpace shouldBe
        Some(
          Space(
            index = indexOf(s"fn>>  <<"),
            text = "  "
          )
        )

      function.postSignatureSpace shouldBe empty
      function.block shouldBe empty
    }

    "unresolved tail code" in {
      val (blahIndex, code) =
        indexCodeOf {
          """fn function() -> ABC
            |>>blah<<
            |""".stripMargin
        }

      val root =
        parseSoft(code)

      val blahIdentifiers =
        root
          .parts
          .collect {
            case blahIdentifier: SoftAST.Identifier if blahIdentifier.code.text == "blah" =>
              blahIdentifier
          }

      blahIdentifiers should have size 1
      val actual = blahIdentifiers.last

      val expected =
        Identifier(
          index = blahIndex,
          text = "blah"
        )

      actual shouldBe expected
    }

    "within another function with incomplete syntax" in {
      val root =
        parseSoft {
          """fn function(
            |  fn
            |}
            |""".stripMargin
        }

      val functions =
        root
          .toNode
          .walk
          .map(_.data)
          .collect {
            case function: SoftAST.Function =>
              function
          }
          .toList

      functions should have size 2

      /**
       * Test first function
       */
      functions.head.signature.fnName shouldBe Identifier("fn >>function<<")

      /**
       * Test second function
       */
      functions.last.fn shouldBe
        Fn(
          indexOf {
            """fn function(
              |  >>fn<<
              |}
              |""".stripMargin
          }
        )

      functions.last.signature.fnName shouldBe
        SoftAST.IdentifierExpected(
          indexOf {
            """fn function(
              |  fn
              |>><<}
              |""".stripMargin
          }
        )

    }
  }

}
