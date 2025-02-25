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
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.OptionValues._

class GroupParserSpec extends AnyWordSpec with Matchers {

  "missing opening paren" in {
    val tuple =
      parseTuple(")")

    tuple.openToken shouldBe SoftAST.TokenExpected(indexOf(">><<)"), Token.OpenParen)
    tuple.closeToken shouldBe CloseParen(">>)<<")
  }

  "missing closing paren" in {
    val tuple =
      parseTuple("(")

    tuple.openToken shouldBe OpenParen(">>(<<")
    tuple.closeToken shouldBe SoftAST.TokenExpected(indexOf("(>><<"), Token.CloseParen)
  }

  "empty tuple" when {
    "without spaces" in {
      val tuple =
        parseTuple("()")

      val expected =
        SoftAST.Group(
          index = indexOf(">>()<<"),
          openToken = OpenParen(">>(<<)"),
          preHeadExpressionSpace = None,
          headExpression = None,
          postHeadExpressionSpace = None,
          tailExpressions = Seq.empty,
          closeToken = CloseParen("(>>)<<)")
        )

      tuple shouldBe expected
    }

    "with spaces" in {
      val tuple =
        parseTuple("( )")

      val expected =
        SoftAST.Group(
          index = indexOf(">>( )<<"),
          openToken = OpenParen(">>(<< )"),
          preHeadExpressionSpace = Some(Space("(>> <<)")),
          headExpression = None,
          postHeadExpressionSpace = None,
          tailExpressions = Seq.empty,
          closeToken = CloseParen("( >>)<<)")
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
      block.headExpression.asInstanceOf[SoftAST.Group[Token.OpenParen.type, Token.CloseParen.type]]

    tuple.openToken shouldBe OpenParen(">>(<<aaa typename")

    tuple.headExpression.value.asInstanceOf[SoftAST.Identifier] shouldBe
      Identifier(
        index = indexOf("(>>aaa<< typename"),
        text = "aaa"
      )

    tuple.closeToken shouldBe SoftAST.TokenExpected(indexOf("(aaa >><<typename"), Token.CloseParen)

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

    tuple.openToken shouldBe OpenParen(">>(<<aaa: typename")

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

    tuple.closeToken shouldBe CloseParen("(aaa: typename>>)<<")
  }

  "valid second type assignment expression exists" in {
    val tuple = parseTuple("(aaa: typename, bbb: type2)")

    tuple.tailExpressions should have size 1
    val bbb = tuple.tailExpressions.head

    bbb.comma shouldBe Comma("(aaa: typename>>,<< bbb: type2)")

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

    tupleTail.comma shouldBe Comma("(aaa: typename>>,<< bbb: (tuple1, tuple2))")

    tupleTail.preExpressionSpace.value shouldBe Space("(aaa: typename,>> <<bbb: (tuple1, tuple2))")

    val bbb = tupleTail.expression.asInstanceOf[SoftAST.TypeAssignment]

    bbb.expressionLeft shouldBe
      Identifier(
        index = indexOf("(aaa: typename, >>bbb<<: (tuple1, tuple2))"),
        text = "bbb"
      )

    // A quick text to check that the tuple is actually a tuple
    bbb.expressionRight shouldBe a[SoftAST.Group[_, _]]
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
          comma = Comma("(a>>,<< (b, c))"),
          preExpressionSpace = Some(Space("(a,>> <<(b, c))")),
          expression = SoftAST.Group(
            index = indexOf("(a, >>(b, c)<<)"),
            openToken = OpenParen("(a, >>(<<b, c))"),
            preHeadExpressionSpace = None,
            headExpression = Some(Identifier("(a, (>>b<<, c))")),
            postHeadExpressionSpace = None,
            tailExpressions = Seq(
              SoftAST.GroupTail(
                index = indexOf("(a, (b>>, c<<))"),
                comma = Comma("(a, (b>>,<< c))"),
                preExpressionSpace = Some(Space("(a, (b,>> <<c))")),
                expression = Identifier("(a, (b, >>c<<))"),
                postExpressionSpace = None
              )
            ),
            closeToken = CloseParen("(a, (b, c>>)<<)")
          ),
          postExpressionSpace = None
        )
    }

    "has missing tuple elements" in {
      val tuple = parseTuple("(a, , ( , z)")

      tuple shouldBe
        SoftAST.Group(
          index = indexOf(">>(a, , ( , z)<<"),
          openToken = OpenParen(">>(<<a, , ( , z)"),
          preHeadExpressionSpace = None,
          headExpression = Some(
            Identifier(
              index = indexOf("(>>a<<, , ( , z)"),
              text = "a"
            )
          ),
          postHeadExpressionSpace = None,
          tailExpressions = Seq(
            SoftAST.GroupTail(
              index = indexOf("(a>>, <<, ( , z)"),
              comma = Comma("(a>>,<< , ( , z)"),
              preExpressionSpace = Some(Space("(a,>> <<, ( , z)")),
              expression = ExpressionExpected("(a, >><<, ( , z)"),
              postExpressionSpace = None
            ),
            SoftAST.GroupTail(
              index = indexOf("(a, >>, ( , z)<<"),
              comma = Comma("(a, >>,<< ( , z)"),
              preExpressionSpace = Some(Space("(a, ,>> <<( , z)")),
              expression = SoftAST.Group(
                index = indexOf("(a, , >>( , z)<<"),
                openToken = OpenParen("(a, , >>(<< , z)"),
                preHeadExpressionSpace = Some(Space("(a, , (>> <<, z)")),
                headExpression = Some(ExpressionExpected("(a, , ( >><<, z)")),
                postHeadExpressionSpace = None,
                tailExpressions = Seq(
                  SoftAST.GroupTail(
                    index = indexOf("(a, , ( >>, z<<)"),
                    comma = Comma("(a, , ( >>,<< z)"),
                    preExpressionSpace = Some(Space("(a, , ( ,>> <<z)")),
                    expression = Identifier("(a, , ( , >>z<<)"),
                    postExpressionSpace = None
                  )
                ),
                closeToken = CloseParen("(a, , ( , z>>)<<")
              ),
              postExpressionSpace = None
            )
          ),
          closeToken = SoftAST.TokenExpected(indexOf("(a, , ( , z)>><<"), Token.CloseParen)
        )
    }

  }

}
