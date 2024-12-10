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
import org.alephium.ralph.lsp.access.util.TestCodeUtil._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.OptionValues._

class TupleSpec extends AnyWordSpec with Matchers {

  "missing opening paren" in {
    val tuple =
      parseTuple(")")

    tuple.openParen shouldBe
      SoftAST.OpenParenExpected(indexOf(">><<)"))

    tuple.closeParen shouldBe
      SoftAST.CloseParen(
        SoftAST.Code(
          index = indexOf(">>)<<"),
          text = Token.CloseParen.lexeme
        )
      )
  }

  "missing closing paren" in {
    val tuple =
      parseTuple("(")

    tuple.openParen shouldBe
      SoftAST.OpenParen(
        SoftAST.Code(
          index = indexOf(">>(<<"),
          text = Token.OpenParen.lexeme
        )
      )

    tuple.closeParen shouldBe
      SoftAST.CloseParenExpected(indexOf("(>><<"))
  }

  "empty tuple" when {
    "without spaces" in {
      val tuple =
        parseTuple("()")

      val expected =
        SoftAST.Tuple(
          index = indexOf(">>()<<"),
          openParen = SoftAST.OpenParen(
            SoftAST.Code(
              index = indexOf(">>(<<)"),
              text = Token.OpenParen.lexeme
            )
          ),
          preHeadExpressionSpace = None,
          headExpression = None,
          postHeadExpressionSpace = None,
          tailExpressions = Seq.empty,
          closeParen = SoftAST.CloseParen(
            SoftAST.Code(
              index = indexOf("(>>)<<)"),
              text = Token.CloseParen.lexeme
            )
          )
        )

      tuple shouldBe expected
    }

    "with spaces" in {
      val tuple =
        parseTuple("( )")

      val expected =
        SoftAST.Tuple(
          index = indexOf(">>( )<<"),
          openParen = SoftAST.OpenParen(
            SoftAST.Code(
              index = indexOf(">>(<< )"),
              text = Token.OpenParen.lexeme
            )
          ),
          preHeadExpressionSpace = Some(
            SoftAST.Space(
              SoftAST.Code(
                index = indexOf("(>> <<)"),
                text = " "
              )
            )
          ),
          headExpression = None,
          postHeadExpressionSpace = None,
          tailExpressions = Seq.empty,
          closeParen = SoftAST.CloseParen(
            SoftAST.Code(
              index = indexOf("( >>)<<)"),
              text = Token.CloseParen.lexeme
            )
          )
        )

      tuple shouldBe expected
    }
  }

  "error tuple item exist" in {
    val body = parseSoft("(aaa typename")
    body.parts should have size 2

    /**
     * First body part is Tuple
     */
    val tuple =
      body.parts.head.part.asInstanceOf[SoftAST.Tuple]

    tuple.openParen shouldBe
      SoftAST.OpenParen(
        SoftAST.Code(
          index = indexOf(">>(<<aaa typename"),
          text = Token.OpenParen.lexeme
        )
      )

    tuple.headExpression.value.asInstanceOf[SoftAST.Identifier] shouldBe
      SoftAST.Identifier(
        SoftAST.Code(
          index = indexOf("(>>aaa<< typename"),
          text = "aaa"
        )
      )

    tuple.closeParen shouldBe SoftAST.CloseParenExpected(indexOf("(aaa >><<typename"))

    /**
     * Second body part is an Identifier
     */
    val identifier =
      body.parts.last.part.asInstanceOf[SoftAST.Identifier]

    identifier shouldBe
      SoftAST.Identifier(
        SoftAST.Code(
          index = indexOf("(aaa >>typename<<"),
          text = "typename"
        )
      )
  }

  "valid type assignment expression exists" in {
    val tuple = parseTuple("(aaa: typename)")

    tuple.openParen shouldBe
      SoftAST.OpenParen(
        SoftAST.Code(
          index = indexOf(">>(<<aaa: typename"),
          text = Token.OpenParen.lexeme
        )
      )

    val headExpression =
      tuple.headExpression.value.asInstanceOf[SoftAST.TypeAssignment]

    headExpression.name shouldBe
      SoftAST.Identifier(
        SoftAST.Code(
          index = indexOf("(>>aaa<<: typename"),
          text = "aaa"
        )
      )

    headExpression.colon shouldBe
      SoftAST.Colon(
        SoftAST.Code(
          index = indexOf("(aaa>>:<< typename"),
          text = Token.Colon.lexeme
        )
      )

    headExpression.postColonSpace.value shouldBe
      SoftAST.Space(
        SoftAST.Code(
          index = indexOf("(aaa:>> <<typename"),
          text = " "
        )
      )

    headExpression.tpe shouldBe
      SoftAST.Identifier(
        SoftAST.Code(
          index = indexOf("(aaa: >>typename<<"),
          text = "typename"
        )
      )

    tuple.closeParen shouldBe
      SoftAST.CloseParen(
        SoftAST.Code(
          index = indexOf("(aaa: typename>>)<<"),
          text = Token.CloseParen.lexeme
        )
      )
  }

  "valid second type assignment expression exists" in {
    val tuple = parseTuple("(aaa: typename, bbb: type2)")

    tuple.tailExpressions should have size 1
    val bbb = tuple.tailExpressions.head

    bbb.comma shouldBe
      SoftAST.Comma(
        SoftAST.Code(
          indexOf("(aaa: typename>>,<< bbb: type2)"),
          Token.Comma.lexeme
        )
      )

    bbb.preExpressionSpace.value shouldBe
      SoftAST.Space(
        SoftAST.Code(
          index = indexOf("(aaa: typename,>> <<bbb: type2)"),
          text = " "
        )
      )

    val bbbTypeAssignment =
      bbb.expression.asInstanceOf[SoftAST.TypeAssignment]

    bbbTypeAssignment.name shouldBe
      SoftAST.Identifier(
        SoftAST.Code(
          index = indexOf("(aaa: typename, >>bbb<<: type2)"),
          text = "bbb"
        )
      )

    bbbTypeAssignment.tpe shouldBe
      SoftAST.Identifier(
        SoftAST.Code(
          index = indexOf("(aaa: typename, bbb: >>type2<<)"),
          text = "type2"
        )
      )

    bbb.postExpressionSpace shouldBe empty
  }

  "second parameter type is a tuple" in {
    val tuple = parseTuple("(aaa: typename, bbb: (tuple1, tuple2))")

    tuple.tailExpressions should have size 1
    val tupleTail = tuple.tailExpressions.head

    tupleTail.comma shouldBe
      SoftAST.Comma(
        SoftAST.Code(
          index = indexOf("(aaa: typename>>,<< bbb: (tuple1, tuple2))"),
          text = Token.Comma.lexeme
        )
      )

    tupleTail.preExpressionSpace.value shouldBe
      SoftAST.Space(
        SoftAST.Code(
          index = indexOf("(aaa: typename,>> <<bbb: (tuple1, tuple2))"),
          text = " "
        )
      )

    val bbb = tupleTail.expression.asInstanceOf[SoftAST.TypeAssignment]

    bbb.name shouldBe
      SoftAST.Identifier(
        SoftAST.Code(
          index = indexOf("(aaa: typename, >>bbb<<: (tuple1, tuple2))"),
          text = "bbb"
        )
      )

    // A quick text to check that the tuple is actually a tuple
    bbb.tpe shouldBe a[SoftAST.Tuple]
    // Convert the tpe to code and it should be the tuple
    bbb.tpe.toCode() shouldBe "(tuple1, tuple2)"
  }

}
