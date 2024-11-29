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

class FunctionNameSpec extends AnyWordSpec with Matchers {

  "lowercase function name" in {
    val function =
      parseFunction("fn function")

    function.signature.fnName shouldBe
      SoftAST.Identifier(
        code = "function",
        index = indexOf("fn >>function<<")
      )
  }

  "randomly cased function name" in {
    val function =
      parseFunction("fn _FnUc_TiOn_")

    function.signature.fnName shouldBe
      SoftAST.Identifier(
        code = "_FnUc_TiOn_",
        index = indexOf("fn >>_FnUc_TiOn_<<")
      )
  }

  "spaces before and after function name" in {
    val function =
      parseFunction("fn       _FnUc_TiOn_        ")

    function.preSignatureSpace shouldBe SoftAST.Space("       ", indexOf("fn>>       <<_FnUc_TiOn_        "))

    function.signature.fnName shouldBe
      SoftAST.Identifier(
        code = "_FnUc_TiOn_",
        index = indexOf("fn       >>_FnUc_TiOn_<<        ")
      )

    function.signature.preParamSpace should contain(SoftAST.Space("        ", indexOf("fn       _FnUc_TiOn_>>        <<")))
  }

  "unresolved characters after function name" in {
    val function =
      parseFunction("fn abcd efgh ijk")

    val expected =
      SoftAST.Identifier(
        code = "abcd",
        index = indexOf("fn >>abcd<< efgh ijk")
      )

    function.signature.fnName shouldBe expected
  }

  "random characters and symbols after function name" in {
    val root =
      parseSoft("fn abcd *YNKJ BUP*P")

    val (functions, unresolved) =
      root
        .parts
        .map(_.part)
        .collect {
          case function: SoftAST.Function =>
            Left(function)

          case unresolve: SoftAST.Unresolved =>
            Right(unresolve)
        }
        .partitionMap(identity)

    /**
     * Test function
     */
    functions should have size 1
    val function = functions.head
    function.signature.fnName shouldBe
      SoftAST.Identifier(
        code = "abcd",
        index = indexOf("fn >>abcd<< *YNKJ BUP*P")
      )

    /**
     * Test Unresolved
     */
    unresolved should have size 2
    // head unresolved
    unresolved.head shouldBe
      SoftAST.Unresolved(
        code = "*YNKJ",
        index = indexOf("fn abcd >>*YNKJ<< BUP*P")
      )
    // last unresolved
    unresolved.last shouldBe
      SoftAST.Unresolved(
        code = "BUP*P",
        index = indexOf("fn abcd *YNKJ >>BUP*P<<")
      )
  }

  "random spaces before and after function name" in {
    val function =
      parseFunction {
        """fn
          |
          |abcd
          |
          |
          |efgh ijk
          |
          |LMNO
          |""".stripMargin
      }

    function.signature.fnName shouldBe
      SoftAST.Identifier(
        code = "abcd",
        index = indexOf("""fn
            |
            |>>abcd<<
            |""".stripMargin)
      )

  }

  "missing closing parentheses" in {
    val function =
      parseFunction("fn abcd(")

    function.signature.fnName shouldBe
      SoftAST.Identifier(
        code = "abcd",
        index = indexOf("fn >>abcd<<(")
      )

    function
      .signature
      .params
      .asInstanceOf[SoftAST.NonEmptyParameterClause]
      .closeParen shouldBe SoftAST.CloseParenExpected(indexOf("fn abcd(>><<"))
  }

  "missing opening parentheses" in {
    val function =
      parseFunction("fn abcd)")

    function.signature.fnName shouldBe
      SoftAST.Identifier(
        code = "abcd",
        index = indexOf("fn >>abcd<<)")
      )

    function
      .signature
      .params
      .asInstanceOf[SoftAST.NonEmptyParameterClause]
      .openParen shouldBe SoftAST.OpenParenExpected(indexOf("fn abcd>><<)"))
  }

}
