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

class FunctionNameSpec extends AnyWordSpec with Matchers {

  "lowercase function name" in {
    val function =
      parseFunction("fn function")

    function.signature.fnName shouldBe
      Identifier(
        index = indexOf("fn >>function<<"),
        text = "function"
      )
  }

  "randomly cased function name" in {
    val function =
      parseFunction("fn _FnUc_TiOn_")

    function.signature.fnName shouldBe
      Identifier(
        index = indexOf("fn >>_FnUc_TiOn_<<"),
        text = "_FnUc_TiOn_"
      )
  }

  "spaces before and after function name" in {
    val function =
      parseFunction("fn       _FnUc_TiOn_        ")

    function.preSignatureSpace shouldBe
      Some(
        Space(
          index = indexOf("fn>>       <<_FnUc_TiOn_        "),
          text = "       "
        )
      )

    function.signature.fnName shouldBe
      Identifier(
        index = indexOf("fn       >>_FnUc_TiOn_<<        "),
        text = "_FnUc_TiOn_"
      )

    function.signature.preParamSpace should
      contain(
        Space(
          index = indexOf("fn       _FnUc_TiOn_>>        <<"),
          text = "        "
        )
      )
  }

  "unresolved characters after function name" in {
    val function =
      parseFunction("fn abcd efgh ijk")

    val expected =
      Identifier(
        index = indexOf("fn >>abcd<< efgh ijk"),
        text = "abcd"
      )

    function.signature.fnName shouldBe expected
  }

  "random characters and symbols after function name" in {
    val root =
      parseSoft("fn abcd *YNKJ *BUPP")

    val (functions, unresolved) =
      root
        .parts
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
      Identifier(
        index = indexOf("fn >>abcd<< *YNKJ *BUPP"),
        text = "abcd"
      )

    /**
     * Test Unresolved
     */
    unresolved should have size 2
    // head unresolved
    unresolved.head shouldBe
      Unresolved(
        index = indexOf("fn abcd >>*YNKJ<< *BUPP"),
        text = "*YNKJ"
      )
    // last unresolved
    unresolved.last shouldBe
      Unresolved(
        index = indexOf("fn abcd *YNKJ >>*BUPP<<"),
        text = "*BUPP"
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
          |""".stripMargin
      }

    function.signature.fnName shouldBe
      Identifier(
        index = indexOf("""fn
            |
            |>>abcd<<
            |""".stripMargin),
        text = "abcd"
      )

  }

  "missing closing parentheses" in {
    val function =
      parseFunction("fn abcd(")

    function.signature.fnName shouldBe
      Identifier(
        index = indexOf("fn >>abcd<<("),
        text = "abcd"
      )

    function
      .signature
      .params
      .closeToken
      .value shouldBe SoftAST.TokenExpected(indexOf("fn abcd(>><<"), Token.CloseParen)
  }

  "missing opening parentheses" in {
    val function =
      parseFunction("fn abcd)")

    function.signature.fnName shouldBe
      Identifier(
        index = indexOf("fn >>abcd<<)"),
        text = "abcd"
      )

    function
      .signature
      .params
      .openToken
      .value shouldBe SoftAST.TokenExpected(indexOf("fn abcd>><<)"), Token.OpenParen)
  }

}
