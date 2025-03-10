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
