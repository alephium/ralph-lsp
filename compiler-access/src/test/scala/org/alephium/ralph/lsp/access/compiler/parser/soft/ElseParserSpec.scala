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
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.TestSoftAST._
import org.alephium.ralph.lsp.access.util.TestCodeUtil._
import org.alephium.ralph.lsp.access.util.TestFastParse._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ElseParserSpec extends AnyWordSpec with Matchers {

  "fail" when {
    "`else` is not a keyword" in {
      assertIsFastParseError {
        parseConst("elsie")
      }
    }
  }

  "pass" when {
    "only `else` is defined" in {
      val elseAST = parseElse("else")

      elseAST shouldBe
        SoftAST.Else(
          index = indexOf(">>else<<"),
          elseToken = Else(">>else<<"),
          preBlockSpace = None,
          block = None
        )
    }

    "fully defined" in {
      val elseAST = parseElse("else{ }")

      elseAST shouldBe
        SoftAST.Else(
          index = indexOf(">>else{ }<<"),
          elseToken = Else(">>else<<{ }"),
          preBlockSpace = None,
          block = Some(
            SoftAST.Block(
              index = indexOf("else>>{ }<<"),
              openCurly = OpenCurly("else>>{<< }"),
              parts = Seq(Space("else{>> <<}")),
              closeCurly = CloseCurly("else{ >>}<<")
            )
          )
        )
    }

    "close curly is missing" in {
      val elseAST = parseElse("else{ ")

      elseAST shouldBe
        SoftAST.Else(
          index = indexOf(">>else{ <<"),
          elseToken = Else(">>else<<{ "),
          preBlockSpace = None,
          block = Some(
            SoftAST.Block(
              index = indexOf("else>>{ <<"),
              openCurly = OpenCurly("else>>{<< "),
              parts = Seq(Space("else{>> <<")),
              closeCurly = TokenExpected("else{ >><<", Token.CloseCurly)
            )
          )
        )
    }
  }

}
