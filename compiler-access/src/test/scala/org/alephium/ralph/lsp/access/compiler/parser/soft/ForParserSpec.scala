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
      root.parts.head shouldBe
        Identifier(
          index = indexOf(">>forloop<<"),
          text = "forloop"
        )
    }
  }

  "succeed" when {
    "for is followed by a `space` boundary" in {
      val forExpression =
        parseFor("for ")

      forExpression shouldBe
        SoftAST.For(
          index = indexOf(">>for <<"),
          forToken = For(indexOf(">>for<< ")),
          postForSpace = Some(SpaceOne(indexOf("for>> <<"))),
          openParen = SoftAST.TokenExpected(indexOf("for >><<"), Token.OpenParen),
          postOpenParenSpace = None,
          expression1 = SoftAST.ExpressionExpected(indexOf("for >><<")),
          postExpression1Space = None,
          postExpression1Semicolon = SoftAST.TokenExpected(indexOf("for >><<"), Token.Semicolon),
          postExpression1SemicolonSpace = None,
          expression2 = SoftAST.ExpressionExpected(indexOf("for >><<")),
          postExpression2Space = None,
          postExpression2Semicolon = SoftAST.TokenExpected(indexOf("for >><<"), Token.Semicolon),
          postExpression2SemicolonSpace = None,
          expression3 = SoftAST.ExpressionExpected(indexOf("for >><<")),
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
          forToken = For(indexOf(">>for<<(")),
          postForSpace = None,
          openParen = OpenParen(indexOf("for>>(<<")),
          postOpenParenSpace = None,
          expression1 = SoftAST.ExpressionExpected(indexOf("for(>><<")),
          postExpression1Space = None,
          postExpression1Semicolon = SoftAST.TokenExpected(indexOf("for(>><<"), Token.Semicolon),
          postExpression1SemicolonSpace = None,
          expression2 = SoftAST.ExpressionExpected(indexOf("for(>><<")),
          postExpression2Space = None,
          postExpression2Semicolon = SoftAST.TokenExpected(indexOf("for(>><<"), Token.Semicolon),
          postExpression2SemicolonSpace = None,
          expression3 = SoftAST.ExpressionExpected(indexOf("for(>><<")),
          postExpression3Space = None,
          closeParen = SoftAST.TokenExpected(indexOf("for(>><<"), Token.CloseParen),
          postCloseParenSpace = None,
          block = None
        )
    }
  }

}
