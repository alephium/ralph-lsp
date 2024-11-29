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
import org.scalatest.EitherValues._

class ReferenceCallSpec extends AnyWordSpec with Matchers {

  "parseOrFail" should {
    "fail" when {
      "open paren is not provided" in {
        val actual =
          parseOrFailReferenceCall("object")
            .left
            .value

        // This error message is from FastParse, not something we use for reporting, but for parser composition.
        // Therefore, the actual FastParse error message is not important here.
        // This test is to ensure that a FastParse failure does occur.
        actual.message shouldBe """Expected "(""""
      }

      "parentheses are provided but reference name is not" in {
        // Checks that a FastParse failure does occur, the actual error message from FastParse is not important.
        parseOrFailReferenceCall("()")
          .left
          .value
      }
    }
  }

  "succeed" when {
    "reference call is without parameters" in {
      val actual =
        parseReferenceCall("object(")

      val expected =
        SoftAST.ReferenceCall(
          index = indexOf(">>object(<<"),
          reference = SoftAST.Identifier("object", indexOf(">>object<<(")),
          preArgumentsSpace = None,
          arguments = SoftAST.Arguments(
            index = indexOf("object>>(<<"),
            openParen = SoftAST.OpenParen(indexOf("object>>(<<")),
            preHeadArgumentSpace = None,
            headArgument = None,
            tailArguments = Seq.empty,
            closeParen = SoftAST.CloseParenExpected(indexOf("object(>><<"))
          )
        )

      actual shouldBe expected
    }

    "reference call is with a parameter" in {
      val actual =
        parseReferenceCall("object(one)")

      val expected =
        SoftAST.ReferenceCall(
          index = indexOf(">>object(one)<<"),
          reference = SoftAST.Identifier("object", indexOf(">>object<<(one)")),
          preArgumentsSpace = None,
          arguments = SoftAST.Arguments(
            index = indexOf("object>>(one)<<"),
            openParen = SoftAST.OpenParen(indexOf("object>>(<<one)")),
            preHeadArgumentSpace = None,
            headArgument = Some(SoftAST.Argument("one", indexOf("object(>>one<<)"))),
            tailArguments = Seq.empty,
            closeParen = SoftAST.CloseParen(indexOf("object(one>>)<<"))
          )
        )

      actual shouldBe expected
    }
  }

}
