// Copyright 2024 The Alephium Authors
// This file is part of the alephium project.
//
// The library is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// The library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with the library. If not, see http://www.gnu.org/licenses/.

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
          .walkDown
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
