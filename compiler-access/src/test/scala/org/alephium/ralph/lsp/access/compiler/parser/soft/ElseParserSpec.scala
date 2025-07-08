// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

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
        parseElse("elsie")
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
          preBodySpace = None,
          body = Right(ExpressionExpected("else>><<"))
        )
    }

    "fully defined with a block" in {
      val elseAST = parseElse("else{ }")

      elseAST shouldBe
        SoftAST.Else(
          index = indexOf(">>else{ }<<"),
          elseToken = Else(">>else<<{ }"),
          preBodySpace = None,
          body = Left(
            SoftAST.Block(
              index = indexOf("else>>{ }<<"),
              openCurly = OpenCurly("else>>{<< }"),
              parts = Seq(Space("else{>> <<}")),
              closeCurly = CloseCurly("else{ >>}<<")
            )
          )
        )
    }

    "fully defined with an unblocked expressions" in {
      val elseAST = parseElse("else blah")

      elseAST shouldBe
        SoftAST.Else(
          index = indexOf(">>else blah<<"),
          elseToken = Else(">>else<< blah"),
          preBodySpace = Some(Space("else>> <<blah")),
          body = Right(Identifier("else >>blah<<"))
        )
    }

    "close curly is missing" in {
      val elseAST = parseElse("else{ ")

      elseAST shouldBe
        SoftAST.Else(
          index = indexOf(">>else{ <<"),
          elseToken = Else(">>else<<{ "),
          preBodySpace = None,
          body = Left(
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
