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

class FunctionReturnClauseSpec extends AnyWordSpec with Matchers {

  "function name is not defined" when {
    "arrow is defined" in {
      val function =
        parseFunction("fn -> ")

      function.signature.returned shouldBe
        SoftAST.FunctionReturn(
          index = indexOf("fn >>-> <<"),
          forwardArrow = SoftAST.ForwardArrow(
            SoftAST.Code(
              index = indexOf("fn >>-><< "),
              text = Token.ForwardArrow.lexeme
            )
          ),
          space = Some(
            SoftAST.Space(
              SoftAST.Code(
                index = indexOf("fn __>> <<"),
                text = " "
              )
            )
          ),
          tpe = SoftAST.IdentifierExpected(indexOf("fn -> >><<"))
        )
    }

    "arrow and type are defined" in {
      val function =
        parseFunction("fn -> type")

      function.signature.returned shouldBe
        SoftAST.FunctionReturn(
          index = indexOf("fn >>-> type<<"),
          forwardArrow = SoftAST.ForwardArrow(
            SoftAST.Code(
              index = indexOf("fn >>-><< type"),
              text = Token.ForwardArrow.lexeme
            )
          ),
          space = Some(
            SoftAST.Space(
              SoftAST.Code(
                index = indexOf("fn __>> <<type"),
                text = " "
              )
            )
          ),
          tpe = SoftAST.Identifier(
            SoftAST.Code(
              index = indexOf("fn -> >>type<<"),
              text = "type"
            )
          )
        )
    }
  }

  "function name is defined without parameter parens" when {
    "arrow is defined" in {
      val function =
        parseFunction("fn function -> ")

      function.signature.fnName shouldBe
        SoftAST.Identifier(
          SoftAST.Code(
            index = indexOf("fn >>function<< -> "),
            text = "function"
          )
        )

      function.signature.returned shouldBe
        SoftAST.FunctionReturn(
          index = indexOf("fn function >>-> <<"),
          forwardArrow = SoftAST.ForwardArrow(
            SoftAST.Code(
              index = indexOf("fn function >>-><< "),
              text = Token.ForwardArrow.lexeme
            )
          ),
          space = Some(
            SoftAST.Space(
              SoftAST.Code(
                index = indexOf("fn function __>> <<"),
                text = " "
              )
            )
          ),
          tpe = SoftAST.IdentifierExpected(indexOf("fn function -> >><<"))
        )
    }

    "arrow and type are defined" in {
      val function =
        parseFunction("fn function -> type")

      function.signature.fnName shouldBe
        SoftAST.Identifier(
          SoftAST.Code(
            index = indexOf("fn >>function<< -> "),
            text = "function"
          )
        )

      function.signature.returned shouldBe
        SoftAST.FunctionReturn(
          index = indexOf("fn function >>-> type<<"),
          forwardArrow = SoftAST.ForwardArrow(
            SoftAST.Code(
              index = indexOf("fn function >>-><< type"),
              text = Token.ForwardArrow.lexeme
            )
          ),
          space = Some(
            SoftAST.Space(
              SoftAST.Code(
                index = indexOf("fn function __>> <<type"),
                text = " "
              )
            )
          ),
          tpe = SoftAST.Identifier(
            SoftAST.Code(
              index = indexOf("fn function -> >>type<<"),
              text = "type"
            )
          )
        )
    }
  }

  "function name is defined with only the parameter open parens" when {
    "arrow is defined" in {
      val function =
        parseFunction("fn function(-> ")

      function.signature.fnName shouldBe
        SoftAST.Identifier(
          SoftAST.Code(
            index = indexOf("fn >>function<<(-> "),
            text = "function"
          )
        )

      function.signature.params.openParen shouldBe
        SoftAST.OpenParen(
          SoftAST.Code(
            index = indexOf("fn function>>(<<-> "),
            text = Token.OpenParen.lexeme
          )
        )

      function.signature.params.closeParen shouldBe
        SoftAST.CloseParenExpected(indexOf("fn function(>><<-> "))

      function.signature.returned shouldBe
        SoftAST.FunctionReturn(
          index = indexOf("fn function(>>-> <<"),
          forwardArrow = SoftAST.ForwardArrow(
            SoftAST.Code(
              index = indexOf("fn function(>>-><< "),
              text = Token.ForwardArrow.lexeme
            )
          ),
          space = Some(
            SoftAST.Space(
              SoftAST.Code(
                index = indexOf("fn function(__>> <<"),
                text = " "
              )
            )
          ),
          tpe = SoftAST.IdentifierExpected(indexOf("fn function(-> >><<"))
        )
    }

    "arrow and type are defined" in {
      val function =
        parseFunction("fn function(-> Type")

      function.signature.fnName shouldBe
        SoftAST.Identifier(
          SoftAST.Code(
            index = indexOf("fn >>function<<(-> Type"),
            text = "function"
          )
        )

      function.signature.params.openParen shouldBe
        SoftAST.OpenParen(
          SoftAST.Code(
            index = indexOf("fn function>>(<<-> Type"),
            text = Token.OpenParen.lexeme
          )
        )

      function.signature.params.closeParen shouldBe
        SoftAST.CloseParenExpected(indexOf("fn function(>><<-> Type"))

      function.signature.returned shouldBe
        SoftAST.FunctionReturn(
          index = indexOf("fn function(>>-> Type<<"),
          forwardArrow = SoftAST.ForwardArrow(
            SoftAST.Code(
              index = indexOf("fn function(>>-><< Type"),
              text = Token.ForwardArrow.lexeme
            )
          ),
          space = Some(
            SoftAST.Space(
              SoftAST.Code(
                index = indexOf("fn function(__>> <<Type"),
                text = " "
              )
            )
          ),
          tpe = SoftAST.Identifier(
            SoftAST.Code(
              index = indexOf("fn function(-> >>Type<<"),
              text = "Type"
            )
          )
        )

    }
  }

  "function name is defined with parameter open parens" when {
    "arrow is defined" in {
      val function =
        parseFunction("fn function() -> ")

      function.signature.fnName shouldBe
        SoftAST.Identifier(
          SoftAST.Code(
            index = indexOf("fn >>function<<() -> "),
            text = "function"
          )
        )

      function.signature.params.openParen shouldBe
        SoftAST.OpenParen(
          SoftAST.Code(
            index = indexOf("fn function>>(<<) -> "),
            text = Token.OpenParen.lexeme
          )
        )

      function.signature.params.closeParen shouldBe
        SoftAST.CloseParen(
          SoftAST.Code(
            index = indexOf("fn function(>>)<< -> "),
            text = Token.CloseParen.lexeme
          )
        )

      function.signature.returned shouldBe
        SoftAST.FunctionReturn(
          index = indexOf("fn function() >>-> <<"),
          forwardArrow = SoftAST.ForwardArrow(
            SoftAST.Code(
              index = indexOf("fn function() >>-><< "),
              text = Token.ForwardArrow.lexeme
            )
          ),
          space = Some(
            SoftAST.Space(
              SoftAST.Code(
                index = indexOf("fn function() __>> <<"),
                text = " "
              )
            )
          ),
          tpe = SoftAST.IdentifierExpected(indexOf("fn function() -> >><<"))
        )
    }

    "arrow and type are defined" in {
      val function =
        parseFunction("fn function() -> type")

      function.signature.fnName shouldBe
        SoftAST.Identifier(
          SoftAST.Code(
            index = indexOf("fn >>function<<() -> type"),
            text = "function"
          )
        )

      function.signature.params.openParen shouldBe
        SoftAST.OpenParen(
          SoftAST.Code(
            index = indexOf("fn function>>(<<) -> type"),
            text = Token.OpenParen.lexeme
          )
        )

      function.signature.params.closeParen shouldBe
        SoftAST.CloseParen(
          SoftAST.Code(
            index = indexOf("fn function(>>)<< -> type"),
            text = Token.CloseParen.lexeme
          )
        )

      function.signature.returned shouldBe
        SoftAST.FunctionReturn(
          index = indexOf("fn function() >>-> type<<"),
          forwardArrow = SoftAST.ForwardArrow(
            SoftAST.Code(
              index = indexOf("fn function() >>-><< type"),
              text = Token.ForwardArrow.lexeme
            )
          ),
          space = Some(
            SoftAST.Space(
              SoftAST.Code(
                index = indexOf("fn function() __>> <<type"),
                text = " "
              )
            )
          ),
          tpe = SoftAST.Identifier(
            SoftAST.Code(
              index = indexOf("fn function() -> >>type<<"),
              text = "type"
            )
          )
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
        SoftAST.Code(
          index = indexOf("fn >>function<<() => ABC"),
          text = "function"
        )
      )

    function.signature.params.openParen shouldBe
      SoftAST.OpenParen(
        SoftAST.Code(
          index = indexOf("fn function>>(<<) => ABC"),
          text = Token.OpenParen.lexeme
        )
      )

    function.signature.params.closeParen shouldBe
      SoftAST.CloseParen(
        SoftAST.Code(
          index = indexOf("fn function(>>)<< => ABC"),
          text = Token.CloseParen.lexeme
        )
      )

    /**
     * Second part is [[SoftAST.Unresolved]] arrow `=>`
     */
    val unresolvedArrow = block.parts(1).part.asInstanceOf[SoftAST.Unresolved]
    unresolvedArrow shouldBe
      SoftAST.Unresolved(
        SoftAST.Code(
          index = indexOf("fn function() >>=><< ABC"),
          text = "=>"
        )
      )

    /**
     * Third part is [[SoftAST.Identifier]] token `ABC`
     */
    val identifier = block.parts(2).part.asInstanceOf[SoftAST.Identifier]
    identifier shouldBe
      SoftAST.Identifier(
        SoftAST.Code(
          index = indexOf("fn function() => >>ABC<<"),
          text = "ABC"
        )
      )
  }

}
