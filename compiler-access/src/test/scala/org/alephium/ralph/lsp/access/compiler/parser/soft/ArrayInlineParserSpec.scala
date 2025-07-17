// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import org.alephium.ralph.lsp.access.compiler.parser.soft.TestParser._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.TestSoftAST._
import org.alephium.ralph.lsp.access.util.TestCodeUtil._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.OptionValues._

class ArrayInlineParserSpec extends AnyWordSpec with Matchers {

  "parse an empty array" when {
    "no elements are provided" in {
      val ast =
        parseArray("[]")

      ast shouldBe
        SoftAST.ArrayInline(
          SoftAST.Group(
            index = indexOf(">>[]<<"),
            openToken = Some(OpenBracket(">>[<<]")),
            preHeadExpressionSpace = None,
            headExpression = None,
            preTailExpressionSpace = None,
            tailExpressions = Seq.empty,
            closeToken = Some(BlockBracket("[>>]<<"))
          )
        )
    }

    "no elements are provided and the closing bracket is missing" in {
      val ast =
        parseArray("[")

      ast shouldBe
        SoftAST.ArrayInline(
          SoftAST.Group(
            index = indexOf(">>[<<"),
            openToken = Some(OpenBracket(">>[<<")),
            preHeadExpressionSpace = None,
            headExpression = None,
            preTailExpressionSpace = None,
            tailExpressions = Seq.empty,
            closeToken = Some(TokenExpected("[>><<", Token.BlockBracket))
          )
        )
    }
  }

  /**
   * **************************
   * Non-empty array test-cases
   * **************************
   */
  "a single element is inserted" when {
    "the element is a number" in {
      val ast =
        parseArray("[0]")

      ast shouldBe
        SoftAST.ArrayInline(
          SoftAST.Group(
            index = indexOf(">>[0]<<"),
            openToken = Some(OpenBracket(">>[<<0]")),
            preHeadExpressionSpace = None,
            headExpression = Some(Number("[>>0<<]")),
            preTailExpressionSpace = None,
            tailExpressions = Seq.empty,
            closeToken = Some(BlockBracket("[0>>]<<"))
          )
        )
    }

    "the element is an identifier" in {
      val ast =
        parseArray("[abc]")

      ast shouldBe
        SoftAST.ArrayInline(
          SoftAST.Group(
            index = indexOf(">>[abc]<<"),
            openToken = Some(OpenBracket(">>[<<abc]")),
            preHeadExpressionSpace = None,
            headExpression = Some(Identifier("[>>abc<<]")),
            preTailExpressionSpace = None,
            tailExpressions = Seq.empty,
            closeToken = Some(BlockBracket("[abc>>]<<"))
          )
        )
    }

    "the element is an some expression with spaces" in {
      val ast =
        parseArray("[ instance.value ]")

      ast shouldBe
        SoftAST.ArrayInline(
          SoftAST.Group(
            index = indexOf(">>[ instance.value ]<<"),
            openToken = Some(OpenBracket(">>[<< instance.value ]")),
            preHeadExpressionSpace = Some(Space("[>> <<instance.value ]")),
            headExpression = Some(
              SoftAST.MethodCall(
                index = indexOf("[ >>instance.value<< ]"),
                leftExpression = Identifier("[ >>instance<<.value ]"),
                preDotSpace = None,
                dot = Dot("[ instance>>.<<value ]"),
                preRightExpressionSpace = None,
                rightExpression = Identifier("[ instance.>>value<< ]")
              )
            ),
            preTailExpressionSpace = Some(Space("[ instance.value>> <<]")),
            tailExpressions = Seq.empty,
            closeToken = Some(BlockBracket("[ instance.value >>]<<"))
          )
        )
    }
  }

  "multiple elements are inserted with a missing element" in {
    val ast =
      parseArray("[0, abc, instance.constant, MyEnum.Value, function(), ]").asInstanceOf[SoftAST.ArrayInline]

    // assert index
    ast.index shouldBe indexOf(">>[0, abc, instance.constant, MyEnum.Value, function(), ]<<")
    // assert brackets
    ast.group.openToken.value shouldBe OpenBracket(">>[<<0, abc, instance.constant, MyEnum.Value, function(), ]")
    ast.group.closeToken.value shouldBe BlockBracket("[0, abc, instance.constant, MyEnum.Value, function(), >>]<<")
    // assert content
    ast.group.toCode() shouldBe "[0, abc, instance.constant, MyEnum.Value, function(), ]"
    // ast element of the expression is an error
    ast.group.expressions.last shouldBe ExpressionExpected("[0, abc, instance.constant, MyEnum.Value, function(), >><<]")
  }

}
