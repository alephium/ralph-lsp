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

class FunctionReturnClauseSpec extends AnyWordSpec with Matchers {

  "function name is not defined" when {
    "arrow is defined" in {
      val function =
        parseFunction("fn -> ")

      function.signature.returned shouldBe
        SoftAST.FunctionReturn(
          index = indexOf("fn >>-> <<"),
          forwardArrow = SoftAST.ForwardArrow(indexOf("fn >>-><< ")),
          space = Some(SoftAST.Space(" ", indexOf("fn __>> <<"))),
          tpe = SoftAST.TypeExpected(indexOf("fn -> >><<"))
        )
    }

    "arrow and type are defined" in {
      val function =
        parseFunction("fn -> type")

      function.signature.returned shouldBe
        SoftAST.FunctionReturn(
          index = indexOf("fn >>-> type<<"),
          forwardArrow = SoftAST.ForwardArrow(indexOf("fn >>-><< type")),
          space = Some(SoftAST.Space(" ", indexOf("fn __>> <<type"))),
          tpe = SoftAST.Type("type", indexOf("fn -> >>type<<"))
        )
    }
  }

  "function name is defined without parameter parens" when {
    "arrow is defined" in {
      val function =
        parseFunction("fn function -> ")

      function.signature.fnName shouldBe
        SoftAST.Identifier(
          code = "function",
          index = indexOf("fn >>function<< -> ")
        )

      function.signature.returned shouldBe
        SoftAST.FunctionReturn(
          index = indexOf("fn function >>-> <<"),
          forwardArrow = SoftAST.ForwardArrow(indexOf("fn function >>-><< ")),
          space = Some(SoftAST.Space(" ", indexOf("fn function __>> <<"))),
          tpe = SoftAST.TypeExpected(indexOf("fn function -> >><<"))
        )
    }

    "arrow and type are defined" in {
      val function =
        parseFunction("fn function -> type")

      function.signature.fnName shouldBe
        SoftAST.Identifier(
          code = "function",
          index = indexOf("fn >>function<< -> ")
        )

      function.signature.returned shouldBe
        SoftAST.FunctionReturn(
          index = indexOf("fn function >>-> type<<"),
          forwardArrow = SoftAST.ForwardArrow(indexOf("fn function >>-><< type")),
          space = Some(SoftAST.Space(" ", indexOf("fn function __>> <<type"))),
          tpe = SoftAST.Type("type", indexOf("fn function -> >>type<<"))
        )
    }
  }

  "function name is defined with only the parameter open parens" when {
    "arrow is defined" in {
      val function =
        parseFunction("fn function(-> ")

      function.signature.fnName shouldBe
        SoftAST.Identifier(
          code = "function",
          index = indexOf("fn >>function<<(-> ")
        )

      val params = function.signature.params.asInstanceOf[SoftAST.NonEmptyParameterClause]
      params.openParen shouldBe SoftAST.OpenParen(indexOf("fn function>>(<<-> "))
      params.closeParen shouldBe SoftAST.CloseParenExpected(indexOf("fn function(>><<-> "))

      function.signature.returned shouldBe
        SoftAST.FunctionReturn(
          index = indexOf("fn function(>>-> <<"),
          forwardArrow = SoftAST.ForwardArrow(indexOf("fn function(>>-><< ")),
          space = Some(SoftAST.Space(" ", indexOf("fn function(__>> <<"))),
          tpe = SoftAST.TypeExpected(indexOf("fn function(-> >><<"))
        )
    }

    "arrow and type are defined" in {
      val function =
        parseFunction("fn function(-> Type")

      function.signature.fnName shouldBe
        SoftAST.Identifier(
          code = "function",
          index = indexOf("fn >>function<<(-> Type")
        )

      val params = function.signature.params.asInstanceOf[SoftAST.NonEmptyParameterClause]
      params.openParen shouldBe SoftAST.OpenParen(indexOf("fn function>>(<<-> Type"))
      params.closeParen shouldBe SoftAST.CloseParenExpected(indexOf("fn function(>><<-> Type"))

      function.signature.returned shouldBe
        SoftAST.FunctionReturn(
          index = indexOf("fn function(>>-> Type<<"),
          forwardArrow = SoftAST.ForwardArrow(indexOf("fn function(>>-><< Type")),
          space = Some(SoftAST.Space(" ", indexOf("fn function(__>> <<Type"))),
          tpe = SoftAST.Type("Type", indexOf("fn function(-> >>Type<<"))
        )

    }
  }

  "function name is defined with parameter open parens" when {
    "arrow is defined" in {
      val function =
        parseFunction("fn function() -> ")

      function.signature.fnName shouldBe
        SoftAST.Identifier(
          code = "function",
          index = indexOf("fn >>function<<() -> ")
        )

      val params = function.signature.params.asInstanceOf[SoftAST.EmptyParameterClause]
      params.openParen shouldBe SoftAST.OpenParen(indexOf("fn function>>(<<) -> "))
      params.closeParen shouldBe SoftAST.CloseParen(indexOf("fn function(>>)<< -> "))

      function.signature.returned shouldBe
        SoftAST.FunctionReturn(
          index = indexOf("fn function() >>-> <<"),
          forwardArrow = SoftAST.ForwardArrow(indexOf("fn function() >>-><< ")),
          space = Some(SoftAST.Space(" ", indexOf("fn function() __>> <<"))),
          tpe = SoftAST.TypeExpected(indexOf("fn function() -> >><<"))
        )
    }

    "arrow and type are defined" in {
      val function =
        parseFunction("fn function() -> type")

      function.signature.fnName shouldBe
        SoftAST.Identifier(
          code = "function",
          index = indexOf("fn >>function<<() -> type")
        )

      val params = function.signature.params.asInstanceOf[SoftAST.EmptyParameterClause]
      params.openParen shouldBe SoftAST.OpenParen(indexOf("fn function>>(<<) -> type"))
      params.closeParen shouldBe SoftAST.CloseParen(indexOf("fn function(>>)<< -> type"))

      function.signature.returned shouldBe
        SoftAST.FunctionReturn(
          index = indexOf("fn function() >>-> type<<"),
          forwardArrow = SoftAST.ForwardArrow(indexOf("fn function() >>-><< type")),
          space = Some(SoftAST.Space(" ", indexOf("fn function() __>> <<type"))),
          tpe = SoftAST.Type("type", indexOf("fn function() -> >>type<<"))
        )
    }

  }

  "invalid function return signature" in {
    val block =
      parseSoft("fn function() => ABC")

    block.parts should have size 3

    /**
     * First part is [[SoftAST.Function]]
     */
    val function = block.parts.head.part.asInstanceOf[SoftAST.Function]

    function.signature.fnName shouldBe
      SoftAST.Identifier(
        code = "function",
        index = indexOf("fn >>function<<() => ABC")
      )

    val params = function.signature.params.asInstanceOf[SoftAST.EmptyParameterClause]
    params.openParen shouldBe SoftAST.OpenParen(indexOf("fn function>>(<<) => ABC"))
    params.closeParen shouldBe SoftAST.CloseParen(indexOf("fn function(>>)<< => ABC"))

    /**
     * Second part is [[SoftAST.Unresolved]] arrow `=>`
     */
    val unresolvedArrow = block.parts(1).part.asInstanceOf[SoftAST.Unresolved]
    unresolvedArrow shouldBe SoftAST.Unresolved("=>", indexOf("fn function() >>=><< ABC"))

    /**
     * Third part is [[SoftAST.Unresolved]] token `ABC`
     */
    val unresolvedToken = block.parts(2).part.asInstanceOf[SoftAST.Unresolved]
    unresolvedToken shouldBe SoftAST.Unresolved("ABC", indexOf("fn function() => >>ABC<<"))
  }

}
