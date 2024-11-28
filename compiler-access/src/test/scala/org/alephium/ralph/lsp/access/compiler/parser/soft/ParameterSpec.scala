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
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST
import org.alephium.ralph.lsp.access.util.TestCodeUtil._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.OptionValues._

class ParameterSpec extends AnyWordSpec with Matchers {

  "missing opening paren" in {
    val parameter = parseParameter(")").asInstanceOf[SoftAST.NonEmptyParameterClause]

    parameter.openParen shouldBe SoftAST.OpenParenExpected(indexOf(">><<)"))
    parameter.closeParen shouldBe SoftAST.CloseParen(indexOf(">>)<<"))
  }

  "missing closing paren" in {
    val parameter = parseParameter("(").asInstanceOf[SoftAST.NonEmptyParameterClause]

    parameter.openParen shouldBe SoftAST.OpenParen(indexOf(">>(<<"))
    parameter.closeParen shouldBe SoftAST.CloseParenExpected(indexOf("(>><<"))
  }

  "no parameters" when {
    "without spaces" in {
      val parameter = parseParameter("()")

      val expected =
        SoftAST.EmptyParameterClause(
          index = indexOf(">>()<<"),
          openParen = SoftAST.OpenParen(indexOf(">>(<<)")),
          preCloseParenSpace = None,
          closeParen = SoftAST.CloseParen(indexOf("(>>)<<)"))
        )

      parameter shouldBe expected
    }

    "with spaces" in {
      val parameter = parseParameter("( )")

      val expected =
        SoftAST.EmptyParameterClause(
          index = indexOf(">>( )<<"),
          openParen = SoftAST.OpenParen(indexOf(">>(<< )")),
          preCloseParenSpace = Some(SoftAST.Space(" ", indexOf("(>> <<)"))),
          closeParen = SoftAST.CloseParen(indexOf("( >>)<<)"))
        )

      parameter shouldBe expected
    }
  }

  "error parameter exist" in {
    val parameter = parseParameter("(aaa typename").asInstanceOf[SoftAST.NonEmptyParameterClause]

    parameter.openParen shouldBe SoftAST.OpenParen(indexOf(">>(<<aaa typename"))
    parameter.headParam.paramName shouldBe SoftAST.Identifier("aaa", indexOf("(>>aaa<< typename"))
    parameter.headParam.colon shouldBe SoftAST.ColonExpected(indexOf("(aaa >><<typename"))

    parameter.headParam.paramType shouldBe
      SoftAST.Type(
        code = "typename",
        index = indexOf("(aaa >>typename<<")
      )

    parameter.closeParen shouldBe SoftAST.CloseParenExpected(indexOf("(aaa typename>><<"))
  }

  "valid parameter exist" in {
    val parameter = parseParameter("(aaa: typename)").asInstanceOf[SoftAST.NonEmptyParameterClause]

    parameter.openParen shouldBe SoftAST.OpenParen(indexOf(">>(<<aaa: typename"))
    parameter.headParam.paramName shouldBe SoftAST.Identifier("aaa", indexOf("(>>aaa<<: typename"))
    parameter.headParam.colon shouldBe SoftAST.Colon(indexOf("(aaa>>:<< typename"))

    parameter.headParam.postColonSpace.value shouldBe SoftAST.Space(" ", indexOf("(aaa:>> <<typename"))

    parameter.headParam.paramType shouldBe
      SoftAST.Type(
        code = "typename",
        index = indexOf("(aaa: >>typename<<")
      )

    parameter.closeParen shouldBe SoftAST.CloseParen(indexOf("(aaa: typename>>)<<"))
  }

  "second parameter exists" in {
    val result = parseParameter("(aaa: typename, bbb: type2)").asInstanceOf[SoftAST.NonEmptyParameterClause]

    result.tailParams should have size 1
    val bbb = result.tailParams.head

    bbb.preCommaSpace shouldBe empty
    bbb.comma shouldBe SoftAST.Comma(indexOf("(aaa: typename>>,<< bbb: type2)"))
    bbb.postCommaSpace.value shouldBe SoftAST.Space(" ", indexOf("(aaa: typename,>> <<bbb: type2)"))
    bbb.parameter.paramName shouldBe SoftAST.Identifier("bbb", indexOf("(aaa: typename, >>bbb<<: type2)"))
    bbb.parameter.paramType shouldBe
      SoftAST.Type(
        code = "type2",
        index = indexOf("(aaa: typename, bbb: >>type2<<)")
      )
  }

  "second parameter type is a tuple" in {
    val result = parseParameter("(aaa: typename, bbb: (tuple1, tuple2))").asInstanceOf[SoftAST.NonEmptyParameterClause]

    result.tailParams should have size 1
    val bbb = result.tailParams.head

    bbb.preCommaSpace shouldBe empty
    bbb.comma shouldBe SoftAST.Comma(indexOf("(aaa: typename>>,<< bbb: (tuple1, tuple2))"))
    bbb.postCommaSpace.value shouldBe SoftAST.Space(" ", indexOf("(aaa: typename,>> <<bbb: (tuple1, tuple2))"))
    bbb.parameter.paramName shouldBe SoftAST.Identifier("bbb", indexOf("(aaa: typename, >>bbb<<: (tuple1, tuple2))"))
    bbb.parameter.paramType shouldBe a[SoftAST.TupledType]
    bbb.parameter.paramType.toCode() shouldBe "(tuple1, tuple2)"
  }

}
