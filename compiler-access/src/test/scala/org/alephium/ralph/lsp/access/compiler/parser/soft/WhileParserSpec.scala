// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import org.alephium.ralph.lsp.access.compiler.parser.soft.TestParser._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.TestSoftAST._
import org.alephium.ralph.lsp.access.util.TestCodeUtil._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class WhileParserSpec extends AnyWordSpec with Matchers {

  "fail" when {
    "`while` is not followed by a boundary" in {
      val root =
        parseSoft("whilel")

      root.parts should have size 1
      root.parts.head shouldBe
        Identifier(
          index = indexOf(">>whilel<<"),
          text = "whilel"
        )
    }
  }

  "pass" when {
    "`while` followed by end-of-line boundary" in {
      val actual =
        parseWhile("while")

      actual shouldBe
        SoftAST.While(
          index = indexOf(">>while<<"),
          whileToken = While(">>while<<"),
          postWhileSpace = None,
          openParen = SoftAST.TokenExpected(indexOf("while>><<"), Token.OpenParen),
          postOpenParenSpace = None,
          expression = ExpressionExpected("while>><<"),
          postExpressionSpace = None,
          closeParen = SoftAST.TokenExpected(indexOf("while>><<"), Token.CloseParen),
          postCloseParenSpace = None,
          block = None
        )
    }

    "`while` followed by space boundary" in {
      val actual =
        parseWhile("while ")

      actual shouldBe
        SoftAST.While(
          index = indexOf(">>while <<"),
          whileToken = While(">>while<< "),
          postWhileSpace = Some(Space("while>> <<")),
          openParen = SoftAST.TokenExpected(indexOf("while >><<"), Token.OpenParen),
          postOpenParenSpace = None,
          expression = ExpressionExpected("while >><<"),
          postExpressionSpace = None,
          closeParen = SoftAST.TokenExpected(indexOf("while >><<"), Token.CloseParen),
          postCloseParenSpace = None,
          block = None
        )
    }

    "`while` followed by open-paren boundary" in {
      val actual =
        parseWhile("while(")

      actual shouldBe
        SoftAST.While(
          index = indexOf(">>while(<<"),
          whileToken = While(">>while<<"),
          postWhileSpace = None,
          openParen = OpenParen("while>>(<<"),
          postOpenParenSpace = None,
          expression = ExpressionExpected("while(>><<"),
          postExpressionSpace = None,
          closeParen = SoftAST.TokenExpected(indexOf("while(>><<"), Token.CloseParen),
          postCloseParenSpace = None,
          block = None
        )
    }
  }

}
