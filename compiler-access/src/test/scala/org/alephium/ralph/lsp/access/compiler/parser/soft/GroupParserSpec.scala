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

class GroupParserSpec extends AnyWordSpec with Matchers {

  "missing opening paren" in {
    val tuple =
      parseTuple(")")

    tuple.openToken.value shouldBe SoftAST.TokenExpected(indexOf(">><<)"), Token.OpenParen)
    tuple.closeToken.value shouldBe CloseParen(">>)<<")
  }

  "missing closing paren" in {
    val tuple =
      parseTuple("(")

    tuple.openToken.value shouldBe OpenParen(">>(<<")
    tuple.closeToken.value shouldBe SoftAST.TokenExpected(indexOf("(>><<"), Token.CloseParen)
  }

  "empty tuple" when {
    "without spaces" in {
      val tuple =
        parseTuple("()")

      val expected =
        SoftAST.Group(
          index = indexOf(">>()<<"),
          openToken = Some(OpenParen(">>(<<)")),
          preHeadExpressionSpace = None,
          headExpression = None,
          preTailExpressionSpace = None,
          tailExpressions = Seq.empty,
          closeToken = Some(CloseParen("(>>)<<)"))
        )

      tuple shouldBe expected
    }

    "with spaces" in {
      val tuple =
        parseTuple("( )")

      val expected =
        SoftAST.Group(
          index = indexOf(">>( )<<"),
          openToken = Some(OpenParen(">>(<< )")),
          preHeadExpressionSpace = Some(Space("(>> <<)")),
          headExpression = None,
          preTailExpressionSpace = None,
          tailExpressions = Seq.empty,
          closeToken = Some(CloseParen("( >>)<<)"))
        )

      tuple shouldBe expected
    }
  }

  "error tuple item exist" in {
    val root = parseSoft("(aaa typename")
    root.parts should have size 1
    val block = root.parts.head.asInstanceOf[SoftAST.ExpressionBlock]

    /**
     * First root part is a Tuple
     */
    val tuple =
      block.headExpression.asInstanceOf[SoftAST.Group[Token.OpenParen.type, Token.CloseParen.type, Token.Comma.type]]

    tuple.openToken.value shouldBe OpenParen(">>(<<aaa typename")

    tuple.headExpression.value.asInstanceOf[SoftAST.Identifier] shouldBe
      Identifier(
        index = indexOf("(>>aaa<< typename"),
        text = "aaa"
      )

    tuple.closeToken.value shouldBe SoftAST.TokenExpected(indexOf("(aaa >><<typename"), Token.CloseParen)

    /**
     * Second root part is an Identifier
     */
    block.tailExpressions should have size 1

    val identifier =
      block.tailExpressions.head.expression.asInstanceOf[SoftAST.Identifier]

    identifier shouldBe
      Identifier(
        index = indexOf("(aaa >>typename<<"),
        text = "typename"
      )
  }

  "valid type assignment expression exists" in {
    val tuple = parseTuple("(aaa: typename)")

    tuple.openToken.value shouldBe OpenParen(">>(<<aaa: typename")

    val headExpression =
      tuple.headExpression.value.asInstanceOf[SoftAST.TypeAssignment]

    headExpression.expressionLeft shouldBe
      Identifier(
        index = indexOf("(>>aaa<<: typename"),
        text = "aaa"
      )

    headExpression.colon shouldBe Colon("(aaa>>:<< typename")

    headExpression.postColonSpace.value shouldBe Space("(aaa:>> <<typename")

    headExpression.expressionRight shouldBe
      Identifier(
        index = indexOf("(aaa: >>typename<<"),
        text = "typename"
      )

    tuple.closeToken.value shouldBe CloseParen("(aaa: typename>>)<<")
  }

  "valid second type assignment expression exists" in {
    val tuple = parseTuple("(aaa: typename, bbb: type2)")

    tuple.tailExpressions should have size 1
    val bbb = tuple.tailExpressions.head

    bbb.delimiter shouldBe Comma("(aaa: typename>>,<< bbb: type2)")

    bbb.preExpressionSpace.value shouldBe Space("(aaa: typename,>> <<bbb: type2)")

    val bbbTypeAssignment =
      bbb.expression.asInstanceOf[SoftAST.TypeAssignment]

    bbbTypeAssignment.expressionLeft shouldBe
      Identifier(
        index = indexOf("(aaa: typename, >>bbb<<: type2)"),
        text = "bbb"
      )

    bbbTypeAssignment.expressionRight shouldBe
      Identifier(
        index = indexOf("(aaa: typename, bbb: >>type2<<)"),
        text = "type2"
      )

    bbb.postExpressionSpace shouldBe empty
  }

  "second parameter type is a tuple" in {
    val tuple = parseTuple("(aaa: typename, bbb: (tuple1, tuple2))")

    tuple.tailExpressions should have size 1
    val tupleTail = tuple.tailExpressions.head

    tupleTail.delimiter shouldBe Comma("(aaa: typename>>,<< bbb: (tuple1, tuple2))")

    tupleTail.preExpressionSpace.value shouldBe Space("(aaa: typename,>> <<bbb: (tuple1, tuple2))")

    val bbb = tupleTail.expression.asInstanceOf[SoftAST.TypeAssignment]

    bbb.expressionLeft shouldBe
      Identifier(
        index = indexOf("(aaa: typename, >>bbb<<: (tuple1, tuple2))"),
        text = "bbb"
      )

    // A quick text to check that the tuple is actually a tuple
    bbb.expressionRight shouldBe a[SoftAST.Group[_, _, _]]
    // Convert the tpe to code and it should be the tuple
    bbb.expressionRight.toCode() shouldBe "(tuple1, tuple2)"
  }

  "nested tuples" when {
    "all tuple elements are defined" in {
      val tuple = parseTuple("(a, (b, c))")

      tuple.headExpression.value shouldBe a[SoftAST.Identifier]

      tuple.tailExpressions should have size 1
      val lastTuple = tuple.tailExpressions.head

      lastTuple shouldBe
        SoftAST.GroupTail(
          index = indexOf("(a>>, (b, c)<<)"),
          delimiter = Comma("(a>>,<< (b, c))"),
          preExpressionSpace = Some(Space("(a,>> <<(b, c))")),
          expression = SoftAST.Group(
            index = indexOf("(a, >>(b, c)<<)"),
            openToken = Some(OpenParen("(a, >>(<<b, c))")),
            preHeadExpressionSpace = None,
            headExpression = Some(Identifier("(a, (>>b<<, c))")),
            preTailExpressionSpace = None,
            tailExpressions = Seq(
              SoftAST.GroupTail(
                index = indexOf("(a, (b>>, c<<))"),
                delimiter = Comma("(a, (b>>,<< c))"),
                preExpressionSpace = Some(Space("(a, (b,>> <<c))")),
                expression = Identifier("(a, (b, >>c<<))"),
                postExpressionSpace = None
              )
            ),
            closeToken = Some(CloseParen("(a, (b, c>>)<<)"))
          ),
          postExpressionSpace = None
        )
    }

    "has missing tuple elements" in {
      val tuple = parseTuple("(a, , ( , z)")

      tuple shouldBe
        SoftAST.Group(
          index = indexOf(">>(a, , ( , z)<<"),
          openToken = Some(OpenParen(">>(<<a, , ( , z)")),
          preHeadExpressionSpace = None,
          headExpression = Some(
            Identifier(
              index = indexOf("(>>a<<, , ( , z)"),
              text = "a"
            )
          ),
          preTailExpressionSpace = None,
          tailExpressions = Seq(
            SoftAST.GroupTail(
              index = indexOf("(a>>, <<, ( , z)"),
              delimiter = Comma("(a>>,<< , ( , z)"),
              preExpressionSpace = Some(Space("(a,>> <<, ( , z)")),
              expression = ExpressionExpected("(a, >><<, ( , z)"),
              postExpressionSpace = None
            ),
            SoftAST.GroupTail(
              index = indexOf("(a, >>, ( , z)<<"),
              delimiter = Comma("(a, >>,<< ( , z)"),
              preExpressionSpace = Some(Space("(a, ,>> <<( , z)")),
              expression = SoftAST.Group(
                index = indexOf("(a, , >>( , z)<<"),
                openToken = Some(OpenParen("(a, , >>(<< , z)")),
                preHeadExpressionSpace = Some(Space("(a, , (>> <<, z)")),
                headExpression = Some(ExpressionExpected("(a, , ( >><<, z)")),
                preTailExpressionSpace = None,
                tailExpressions = Seq(
                  SoftAST.GroupTail(
                    index = indexOf("(a, , ( >>, z<<)"),
                    delimiter = Comma("(a, , ( >>,<< z)"),
                    preExpressionSpace = Some(Space("(a, , ( ,>> <<z)")),
                    expression = Identifier("(a, , ( , >>z<<)"),
                    postExpressionSpace = None
                  )
                ),
                closeToken = Some(CloseParen("(a, , ( , z>>)<<"))
              ),
              postExpressionSpace = None
            )
          ),
          closeToken = Some(SoftAST.TokenExpected(indexOf("(a, , ( , z)>><<"), Token.CloseParen))
        )
    }

    "the element is an infix expression" in {
      val tuple = parseTuple("((1 + 2) * 3)")

      tuple shouldBe
        SoftAST.Group(
          index = indexOf(">>((1 + 2) * 3)<<"),
          openToken = Some(OpenParen(">>(<<(1 + 2) * 3)")),
          preHeadExpressionSpace = None,
          headExpression = Some(
            SoftAST.InfixExpression(
              index = indexOf("(>>(1 + 2) * 3<<)"),
              leftExpression = SoftAST.Group(
                index = indexOf("(>>(1 + 2)<< * 3)"),
                openToken = Some(OpenParen("(>>(<<1 + 2) * 3)")),
                preHeadExpressionSpace = None,
                headExpression = Some(
                  SoftAST.InfixExpression(
                    index = indexOf("((>>1 + 2<<) * 3)"),
                    leftExpression = Number("((>>1<< + 2) * 3)"),
                    preOperatorSpace = Some(Space("((1>> <<+ 2) * 3)")),
                    operator = Plus("((1 >>+<< 2) * 3)"),
                    postOperatorSpace = Some(Space("((1 +>> <<2) * 3)")),
                    rightExpression = Number("((1 + >>2<<) * 3)")
                  )
                ),
                preTailExpressionSpace = None,
                tailExpressions = Seq.empty,
                closeToken = Some(CloseParen("((1 + 2>>)<< * 3)"))
              ),
              preOperatorSpace = Some(Space("((1 + 2)>> <<* 3)")),
              operator = Asterisk("((1 + 2) >>*<< 3)"),
              postOperatorSpace = Some(Space("((1 + 2) *>> <<3)")),
              rightExpression = Number("((1 + 2) * >>3<<)")
            )
          ),
          preTailExpressionSpace = None,
          tailExpressions = Seq.empty,
          closeToken = Some(CloseParen("((1 + 2) * 3>>)<<"))
        )

    }

  }

}
