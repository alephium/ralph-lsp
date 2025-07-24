// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import org.alephium.ralph.lsp.access.compiler.parser.soft.TestParser._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.TestSoftAST._
import org.alephium.ralph.lsp.access.util.TestCodeUtil._
import org.alephium.ralph.lsp.access.util.TestFastParse.assertIsFastParseError
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.OptionValues._

class AssetApprovalParserSpec extends AnyWordSpec with Matchers {

  "fail" when {
    "block is not provided" in {
      assertIsFastParseError {
        parseAssetApproval("asset -> ALPH: 1000")
      }
    }
  }

  "Empty Block: Statements are missing" in {
    val ast =
      parseAssetApproval("{}")

    ast shouldBe
      SoftAST.AssetApproval(
        SoftAST.Group(
          index = indexOf(">>{}<<"),
          openToken = Some(OpenCurly(">>{<<}")),
          preHeadExpressionSpace = None,
          headExpression = Some(ExpressionExpected("{>><<}")),
          preTailExpressionSpace = None,
          tailExpressions = Seq.empty,
          closeToken = Some(CloseCurly("{>>}<<"))
        )
      )
  }

  "One asset group is defined" in {
    val ast =
      parseAssetApproval("{user -> asset: amount}")

    ast shouldBe
      SoftAST.AssetApproval(
        SoftAST.Group(
          index = indexOf(">>{user -> asset: amount}<<"),
          openToken = Some(OpenCurly(">>{<<user -> asset: amount}")),
          preHeadExpressionSpace = None,
          headExpression = Some(
            SoftAST.Group(
              index = indexOf("{>>user -> asset: amount<<}"),
              openToken = None,
              preHeadExpressionSpace = None,
              headExpression = Some(
                SoftAST.ArrowAssignment(
                  index = indexOf("{>>user -> asset: amount<<}"),
                  leftExpression = Identifier("{>>user<< -> asset: amount}"),
                  preArrowSpace = Some(Space("{user>> <<-> asset: amount}")),
                  forwardArrow = ForwardArrow("{user >>-><< asset: amount}"),
                  preRightExpressionSpace = Some(Space("{user - >> <<asset: amount}")),
                  rightExpression = SoftAST.TypeAssignment(
                    index = indexOf("{user -> >>asset: amount<<}"),
                    annotations = Seq.empty,
                    expressionLeft = Identifier("{user -> >>asset<<: amount}"),
                    preColonSpace = None,
                    colon = Colon("{user -> asset>>:<< amount}"),
                    postColonSpace = Some(Space("{user -> asset:>> <<amount}")),
                    expressionRight = Identifier("{user -> asset: >>amount<<}")
                  )
                )
              ),
              preTailExpressionSpace = None,
              tailExpressions = Seq.empty,
              closeToken = None
            )
          ),
          preTailExpressionSpace = None,
          tailExpressions = Seq.empty,
          closeToken = Some(CloseCurly("{user -> asset: amount>>}<<"))
        )
      )
  }

  "two asset groups are defined" in {
    val ast =
      parseAssetApproval("{user -> asset: amount; user -> asset: amount}")

    // There are no errors
    ast.assertNoErrors()

    // Total number of expressions
    ast.assets.expressions should have size 2

    // Left expression
    ast.assets.headExpression.value.toCode() shouldBe "user -> asset: amount"

    // Right expressions
    ast.assets.tailExpressions should have size 1
    ast.assets.tailExpressions.head.toCode() shouldBe "; user -> asset: amount"
  }

  // FIXME: To resolve in the next PR with the second part of the issue https://github.com/alephium/ralph-lsp/issues/546
  "two asset groups, each with multiple subgroups are defined" ignore {
    val ast =
      parseAssetApproval(
        """{
          |  user0 -> ALPH: amount00, tokenId: amount01;
          |  user1 -> ALPH: amount10, tokenId: amount11
          |}""".stripMargin
      )

    // There are no errors
    ast.assertNoErrors()

    // Total number of expressions
    ast.assets.expressions should have size 2

    /**
     * ********************
     * **** Head Group ****
     * ********************
     */
    ast.assets.headExpression.value.toCode() shouldBe "user0 -> ALPH: amount00, tokenId: amount01"
    val headGroup = ast.assets.headExpression.value.asInstanceOf[SoftAST.Group[Nothing, Nothing, Token.Comma.type]]
    // Assert that comma actually delimits the head-group
    headGroup.openToken shouldBe empty
    headGroup.closeToken shouldBe empty
    headGroup.tailExpressions.head.delimiter shouldBe
      Comma(
        """{
          |  user0 -> ALPH: amount00>>,<< tokenId: amount01;
          |  user1 -> ALPH: amount10, tokenId: amount11
          |}""".stripMargin
      )
    // head group itself has two expressions
    headGroup.expressions should have size 2
    headGroup.headExpression.value.toCode() shouldBe "user0 -> ALPH: amount00"
    headGroup.tailExpressions should have size 1
    headGroup.tailExpressions.head.toCode() shouldBe ", tokenId: amount01"

    /**
     * ********************
     * **** Tail Group ****
     * ********************
     */
    ast.assets.tailExpressions should have size 1

    ast.assets.tailExpressions.head.toCode() shouldBe
      """;
        |  user1 -> ALPH: amount10, tokenId: amount11
        |""".stripMargin

    val tailGroup = ast.assets.tailExpressions.head.expression.asInstanceOf[SoftAST.Group[Nothing, Nothing, Token.Comma.type]]
    // Assert that comma actually delimits the tail-group
    tailGroup.openToken shouldBe empty
    tailGroup.closeToken shouldBe empty
    tailGroup.tailExpressions.head.delimiter shouldBe
      Comma(
        """{
          |  user0 -> ALPH: amount00, tokenId: amount01;
          |  user1 -> ALPH: amount10>>,<< tokenId: amount11
          |}""".stripMargin
      )
    // head group itself has two expressions
    tailGroup.expressions should have size 2
    tailGroup.headExpression.value.toCode() shouldBe "user1 -> ALPH: amount10"
    tailGroup.tailExpressions should have size 1
    tailGroup.tailExpressions.head.toCode() shouldBe s", tokenId: amount11${Token.Newline.lexeme}"
  }

  "boundary test (Issue #546)" in {
    val ast = parseAssetApproval("{1 alph}")

    ast shouldBe
      SoftAST.AssetApproval(
        assets = SoftAST.Group(
          index = indexOf(">>{1 alph}<<"),
          openToken = Some(OpenCurly(">>{<<1 alph}")),
          preHeadExpressionSpace = None,
          headExpression = Some(
            SoftAST.Group(
              index = indexOf("{>>1 alph<<}"),
              openToken = None,
              preHeadExpressionSpace = None,
              headExpression = Some(
                SoftAST.Number(
                  index = indexOf("{>>1 alph<<}"),
                  documentation = None,
                  number = CodeString("{>>1<< alph}"),
                  unit = Some(
                    SoftAST.UnitAlph(
                      index = indexOf("{1>> alph<<}"),
                      space = Some(Space("{1>> <<alph}")),
                      unit = AlphLowercase("{1 >>alph<<}")
                    )
                  )
                )
              ),
              preTailExpressionSpace = None,
              tailExpressions = Seq.empty,
              closeToken = None
            )
          ),
          preTailExpressionSpace = None,
          tailExpressions = Seq.empty,
          closeToken = Some(CloseCurly("{1 alph>>}<<"))
        )
      )
  }

}
