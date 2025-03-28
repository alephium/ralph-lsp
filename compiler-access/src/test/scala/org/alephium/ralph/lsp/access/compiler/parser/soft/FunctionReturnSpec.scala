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

class FunctionReturnSpec extends AnyWordSpec with Matchers {

  "function name is not defined" when {
    "arrow is defined" in {
      val function =
        parseFunction("fn -> ")

      function.signature.returned shouldBe
        SoftAST.FunctionReturn(
          index = indexOf("fn >>-> <<"),
          forwardArrow = ForwardArrow("fn >>-><< "),
          space = Some(Space("fn __>> <<")),
          tpe = ExpressionExpected("fn -> >><<")
        )
    }

    "arrow and type are defined" in {
      val function =
        parseFunction("fn -> type")

      function.signature.returned shouldBe
        SoftAST.FunctionReturn(
          index = indexOf("fn >>-> type<<"),
          forwardArrow = ForwardArrow("fn >>-><< type"),
          space = Some(
            SoftAST.Space(
              SoftAST.CodeString(
                index = indexOf("fn __>> <<type"),
                text = " "
              )
            )
          ),
          tpe = Identifier(
            index = indexOf("fn -> >>type<<"),
            text = "type"
          )
        )
    }
  }

  "function name is defined without parameter parens" when {
    "arrow is defined" in {
      val function =
        parseFunction("fn function -> ")

      function.signature.fnName shouldBe
        Identifier(
          index = indexOf("fn >>function<< -> "),
          text = "function"
        )

      function.signature.returned shouldBe
        SoftAST.FunctionReturn(
          index = indexOf("fn function >>-> <<"),
          forwardArrow = ForwardArrow("fn function >>-><< "),
          space = Some(Space("fn function __>> <<")),
          tpe = ExpressionExpected("fn function -> >><<")
        )
    }

    "arrow and type are defined" in {
      val function =
        parseFunction("fn function -> type")

      function.signature.fnName shouldBe
        Identifier(
          index = indexOf("fn >>function<< -> "),
          text = "function"
        )

      function.signature.returned shouldBe
        SoftAST.FunctionReturn(
          index = indexOf("fn function >>-> type<<"),
          forwardArrow = ForwardArrow("fn function >>-><< type"),
          space = Some(Space("fn function __>> <<type")),
          tpe = Identifier(
            index = indexOf("fn function -> >>type<<"),
            text = "type"
          )
        )
    }
  }

  "function name is defined with only the parameter open parens" when {
    "arrow is defined" in {
      val function =
        parseFunction("fn function(-> ")

      function.signature.fnName shouldBe
        Identifier(
          index = indexOf("fn >>function<<(-> "),
          text = "function"
        )

      function.signature.params.openToken.value shouldBe OpenParen("fn function>>(<<-> ")

      function.signature.params.closeToken.value shouldBe
        SoftAST.TokenExpected(indexOf("fn function(>><<-> "), Token.CloseParen)

      function.signature.returned shouldBe
        SoftAST.FunctionReturn(
          index = indexOf("fn function(>>-> <<"),
          forwardArrow = ForwardArrow("fn function(>>-><< "),
          space = Some(Space("fn function(__>> <<")),
          tpe = ExpressionExpected("fn function(-> >><<")
        )
    }

    "arrow and type are defined" in {
      val function =
        parseFunction("fn function(-> Type")

      function.signature.fnName shouldBe
        Identifier(
          index = indexOf("fn >>function<<(-> Type"),
          text = "function"
        )

      function.signature.params.openToken.value shouldBe OpenParen("fn function>>(<<-> Type")
      function.signature.params.closeToken.value shouldBe SoftAST.TokenExpected(indexOf("fn function(>><<-> Type"), Token.CloseParen)

      function.signature.returned shouldBe
        SoftAST.FunctionReturn(
          index = indexOf("fn function(>>-> Type<<"),
          forwardArrow = ForwardArrow("fn function(>>-><< Type"),
          space = Some(Space("fn function(__>> <<Type")),
          tpe = Identifier(
            index = indexOf("fn function(-> >>Type<<"),
            text = "Type"
          )
        )

    }
  }

  "function name is defined with parameter open parens" when {
    "arrow is defined" in {
      val function =
        parseFunction("fn function() -> ")

      function.signature.fnName shouldBe
        Identifier(
          index = indexOf("fn >>function<<() -> "),
          text = "function"
        )

      function.signature.params.openToken.value shouldBe OpenParen("fn function>>(<<) -> ")
      function.signature.params.closeToken.value shouldBe CloseParen("fn function(>>)<< -> ")

      function.signature.returned shouldBe
        SoftAST.FunctionReturn(
          index = indexOf("fn function() >>-> <<"),
          forwardArrow = ForwardArrow("fn function() >>-><< "),
          space = Some(Space("fn function() __>> <<")),
          tpe = ExpressionExpected("fn function() -> >><<")
        )
    }

    "arrow and type are defined" in {
      val function =
        parseFunction("fn function() -> type")

      function.signature.fnName shouldBe
        Identifier(
          index = indexOf("fn >>function<<() -> type"),
          text = "function"
        )

      function.signature.params.openToken.value shouldBe OpenParen("fn function>>(<<) -> type")
      function.signature.params.closeToken.value shouldBe CloseParen("fn function(>>)<< -> type")

      function.signature.returned shouldBe
        SoftAST.FunctionReturn(
          index = indexOf("fn function() >>-> type<<"),
          forwardArrow = ForwardArrow("fn function() >>-><< type"),
          space = Some(Space("fn function() __>> <<type")),
          tpe = Identifier(
            index = indexOf("fn function() -> >>type<<"),
            text = "type"
          )
        )
    }

  }

  "invalid function return signature" in {
    val block =
      parseSoft("fn function() => ABC")

    block.parts should have size 4

    /**
     * First part is [[SoftAST.Function]]
     */
    val function = block.parts.head.asInstanceOf[SoftAST.Function]

    function.signature.fnName shouldBe
      Identifier(
        index = indexOf("fn >>function<<() => ABC"),
        text = "function"
      )

    function.signature.params.openToken.value shouldBe OpenParen("fn function>>(<<) => ABC")
    function.signature.params.closeToken.value shouldBe CloseParen("fn function(>>)<< => ABC")

    /**
     * Second part is [[SoftAST.Unresolved]] arrow `=>`
     */
    val unresolvedArrow = block.parts(1).asInstanceOf[SoftAST.Unresolved]
    unresolvedArrow shouldBe
      Unresolved(
        index = indexOf("fn function() >>=><< ABC"),
        text = "=>"
      )

    /**
     * Third part is [[Identifier]] token `ABC`
     */
    val identifier = block.parts(3).asInstanceOf[SoftAST.Identifier]
    identifier shouldBe
      Identifier(
        index = indexOf("fn function() => >>ABC<<"),
        text = "ABC"
      )
  }

}
