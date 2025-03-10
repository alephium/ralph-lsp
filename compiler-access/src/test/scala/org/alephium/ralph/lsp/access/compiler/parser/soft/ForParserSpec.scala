// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import org.alephium.ralph.lsp.access.compiler.parser.soft.TestParser._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.TestSoftAST._
import org.alephium.ralph.lsp.access.util.TestCodeUtil._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ForParserSpec extends AnyWordSpec with Matchers {

  "fail" when {
    "for is not followed a boundary" in {
      val root =
        parseSoft("forloop")

      root.parts should have size 1
      root.parts.head shouldBe Identifier(">>forloop<<")
    }
  }

  "succeed" when {
    "for is followed by a `space` boundary" in {
      val forExpression =
        parseFor("for ")

      forExpression shouldBe
        SoftAST.For(
          index = indexOf(">>for <<"),
          forToken = For(">>for<< "),
          postForSpace = Some(Space("for>> <<")),
          openParen = SoftAST.TokenExpected(indexOf("for >><<"), Token.OpenParen),
          postOpenParenSpace = None,
          expression1 = ExpressionExpected("for >><<"),
          postExpression1Space = None,
          postExpression1Semicolon = SoftAST.TokenExpected(indexOf("for >><<"), Token.Semicolon),
          postExpression1SemicolonSpace = None,
          expression2 = ExpressionExpected("for >><<"),
          postExpression2Space = None,
          postExpression2Semicolon = SoftAST.TokenExpected(indexOf("for >><<"), Token.Semicolon),
          postExpression2SemicolonSpace = None,
          expression3 = ExpressionExpected("for >><<"),
          postExpression3Space = None,
          closeParen = SoftAST.TokenExpected(indexOf("for >><<"), Token.CloseParen),
          postCloseParenSpace = None,
          block = None
        )
    }

    "for is followed by a `open-paren` boundary" in {
      val forExpression =
        parseFor("for(")

      forExpression shouldBe
        SoftAST.For(
          index = indexOf(">>for(<<"),
          forToken = For(">>for<<("),
          postForSpace = None,
          openParen = OpenParen("for>>(<<"),
          postOpenParenSpace = None,
          expression1 = ExpressionExpected("for(>><<"),
          postExpression1Space = None,
          postExpression1Semicolon = SoftAST.TokenExpected(indexOf("for(>><<"), Token.Semicolon),
          postExpression1SemicolonSpace = None,
          expression2 = ExpressionExpected("for(>><<"),
          postExpression2Space = None,
          postExpression2Semicolon = SoftAST.TokenExpected(indexOf("for(>><<"), Token.Semicolon),
          postExpression2SemicolonSpace = None,
          expression3 = ExpressionExpected("for(>><<"),
          postExpression3Space = None,
          closeParen = SoftAST.TokenExpected(indexOf("for(>><<"), Token.CloseParen),
          postCloseParenSpace = None,
          block = None
        )
    }
  }

}
