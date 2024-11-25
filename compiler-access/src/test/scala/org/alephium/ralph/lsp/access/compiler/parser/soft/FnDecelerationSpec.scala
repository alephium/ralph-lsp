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
import org.alephium.ralph.lsp.access.util.TestCodeUtil._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class FnDecelerationSpec extends AnyWordSpec with Matchers {

  "report missing function identifier" when {
    "without spaces" in {
      val function =
        parseFunction("fn")

      function.fn shouldBe SoftAST.Fn(indexOf(">>fn<<"))
      function.preSignatureSpace shouldBe SoftAST.SpaceExpected(indexOf("fn>><<"))
      function.postSignatureSpace shouldBe empty
      function.block shouldBe empty
    }

    "with head space" in {
      val functions =
        parseSoft(" fn")
          .parts
          .map(_.part)
          .collect {
            case function: SoftAST.Function =>
              function
          }

      functions should have size 1
      val function = functions.head

      function.fn shouldBe SoftAST.Fn(indexOf(s" >>fn<<"))
      function.preSignatureSpace shouldBe SoftAST.SpaceExpected(indexOf(s" fn>><<"))
      function.postSignatureSpace shouldBe empty
      function.block shouldBe empty
    }

    "with tail space" in {
      val function =
        parseFunction("fn  ")

      function.fn shouldBe SoftAST.Fn(indexOf(s">>fn<< "))
      function.preSignatureSpace shouldBe SoftAST.Space("  ", indexOf(s"fn>>  <<"))
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

      val body =
        parseSoft(code)

      val allUnresolved =
        body
          .parts
          .map(_.part)
          .collect {
            case unresolved: SoftAST.Unresolved =>
              unresolved
          }

      allUnresolved should have size 1
      val actual = allUnresolved.last

      val expected =
        SoftAST.Unresolved(
          code = "blah",
          index = blahIndex
        )

      actual shouldBe expected
    }

    "within another function with invalid syntax" in {
      val root =
        parseSoft {
          """fn function(
            |  fn
            |}
            |""".stripMargin
        }

      val (functions, unresolved) =
        root
          .parts
          .map(_.part)
          .collect {
            case function: SoftAST.Function =>
              Left(function)

            case unresolved: SoftAST.Unresolved =>
              Right(unresolved)
          }
          .partitionMap(identity)

      /**
       * Test function
       */
      functions should have size 1
      val function = functions.head
      function.signature.fnName shouldBe
        SoftAST.Identifier(
          code = "function",
          index = indexOf("fn >>function<<")
        )

      /**
       * Test Unresolved
       */
      unresolved should have size 1
      unresolved.head shouldBe
        SoftAST.Unresolved(
          code = "}",
          index = indexOf {
            """fn function(
              |  fn
              |>>}<<
              |""".stripMargin
          }
        )
    }
  }

}
