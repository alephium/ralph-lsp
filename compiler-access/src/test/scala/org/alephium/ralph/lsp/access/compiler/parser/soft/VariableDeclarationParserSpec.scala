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

class VariableDeclarationParserSpec extends AnyWordSpec with Matchers {

  "succeed" when {
    "full valid variable declaration is defined" in {
      val varDec =
        parseVariableDeclaration("let mut variable = 1")

      varDec shouldBe
        SoftAST.VariableDeclaration(
          index = indexOf(">>let mut variable = 1<<"),
          let = Let(">>let<< mut variable = 1"),
          postLetSpace = Some(Space("let>> <<mut variable = 1")),
          assignment = SoftAST.Assignment(
            index = indexOf("let >>mut variable = 1<<"),
            expressionLeft = SoftAST.MutableBinding(
              index = indexOf("let >>mut variable<< = 1"),
              mut = Mut("let >>mut<< variable = 1"),
              space = Some(Space("let mut>> <<variable = 1")),
              identifier = Identifier("let mut >>variable<< = 1")
            ),
            postIdentifierSpace = Some(Space("let mut variable>> <<= 1")),
            equalToken = Equal("let mut variable >>=<< 1"),
            postEqualSpace = Some(Space("let mut variable =>> <<1")),
            expressionRight = Number(indexOf("let mut variable = >>1<<"), "1")
          )
        )
    }
  }

  "let" should {
    "always result in variable-declaration" when {
      "right expression is missing" in {
        val varDec =
          parseVariableDeclaration("let mut variable = ")

        varDec shouldBe
          SoftAST.VariableDeclaration(
            index = indexOf(">>let mut variable = <<"),
            let = Let(">>let<< mut variable = "),
            postLetSpace = Some(Space("let>> <<mut variable = ")),
            assignment = SoftAST.Assignment(
              index = indexOf("let >>mut variable = <<"),
              expressionLeft = SoftAST.MutableBinding(
                index = indexOf("let >>mut variable<< = "),
                mut = Mut("let >>mut<< variable = "),
                space = Some(Space("let mut>> <<variable = ")),
                identifier = Identifier("let mut >>variable<< = ")
              ),
              postIdentifierSpace = Some(Space("let mut variable>> <<= ")),
              equalToken = Equal("let mut variable >>=<< "),
              postEqualSpace = Some(Space("let mut variable =>> <<")),
              expressionRight = ExpressionExpected("let mut variable = >><<")
            )
          )
      }
    }

    "equal is missing" in {
      val varDec =
        parseVariableDeclaration("let mut variable")

      varDec shouldBe
        SoftAST.VariableDeclaration(
          index = indexOf(">>let mut variable<<"),
          let = Let(">>let<< mut variable"),
          postLetSpace = Some(Space("let>> <<mut variable")),
          assignment = SoftAST.Assignment(
            index = indexOf("let >>mut variable<<"),
            expressionLeft = SoftAST.MutableBinding(
              index = indexOf("let >>mut variable<<"),
              mut = Mut("let >>mut<< variable"),
              space = Some(Space("let mut>> <<variable")),
              identifier = Identifier("let mut >>variable<<")
            ),
            postIdentifierSpace = None,
            equalToken = SoftAST.TokenExpected(indexOf("let mut variable>><<"), Token.Equal),
            postEqualSpace = None,
            expressionRight = ExpressionExpected("let mut variable>><<")
          )
        )
    }

    "variable name is missing" in {
      val varDec =
        parseVariableDeclaration("let mut")

      varDec shouldBe
        SoftAST.VariableDeclaration(
          index = indexOf(">>let mut<<"),
          let = Let(">>let<< mut"),
          postLetSpace = Some(Space("let>> <<mut")),
          assignment = SoftAST.Assignment(
            index = indexOf("let >>mut<<"),
            expressionLeft = SoftAST.MutableBinding(
              index = indexOf("let >>mut<<"),
              mut = Mut("let >>mut<<"),
              space = None,
              identifier = IdentifierExpected("let mut>><<")
            ),
            postIdentifierSpace = None,
            equalToken = SoftAST.TokenExpected(indexOf("let mut>><<"), Token.Equal),
            postEqualSpace = None,
            expressionRight = ExpressionExpected("let mut>><<")
          )
        )
    }

    "only let is defined" in {
      val varDec =
        parseVariableDeclaration("let")

      varDec shouldBe
        SoftAST.VariableDeclaration(
          index = indexOf(">>let<<"),
          let = Let(">>let<<"),
          postLetSpace = None,
          assignment = SoftAST.Assignment(
            index = indexOf("let>><<"),
            expressionLeft = ExpressionExpected("let>><<"),
            postIdentifierSpace = None,
            equalToken = SoftAST.TokenExpected(indexOf("let>><<"), Token.Equal),
            postEqualSpace = None,
            expressionRight = ExpressionExpected("let>><<")
          )
        )
    }
  }

  "`let` or any other reserved keyword" should {
    "not be allowed as variable name" in {
      Token.reserved foreach {
        reserved =>
          val variable =
            parseSoft(s"let ${reserved.lexeme} = 1")

          // Variable declaration should be defined because `let` is defined.
          // But none of the left expressions should be an identifier
          variable.toNode.walkDown.map(_.data).collect {
            case variableDeclaration: SoftAST.VariableDeclaration =>
              variableDeclaration.assignment.expressionLeft should not be a[SoftAST.Identifier]
              variableDeclaration
          } should not be empty

          // the tree should not contain any identifiers.
          variable.toNode.walkDown.map(_.data).collect {
            case ident: SoftAST.Identifier =>
              ident
          } shouldBe empty

      }
    }

    "allow letter as variable name" in {
      val varDec =
        parseVariableDeclaration("let letter = 1")

      varDec.assignment.expressionLeft.asInstanceOf[SoftAST.Identifier].code.text shouldBe "letter"
    }
  }

  "allow expressions as assignment identifiers" when {
    "the identifier is a tuple" in {
      val tupleVarDec =
        parseVariableDeclaration("let (a, b, c) = blah")

      tupleVarDec.let shouldBe Let(">>let<< (a, b, c) = blah")
      tupleVarDec.postLetSpace shouldBe Some(Space("let>> <<(a, b, c) = blah"))

      // left is a tuple
      val left = tupleVarDec.assignment.expressionLeft.asInstanceOf[SoftAST.Group[Token.OpenParen.type, Token.CloseParen.type]]
      left.index shouldBe indexOf("let >>(a, b, c)<< = blah")
      left.toCode() shouldBe "(a, b, c)"

      // right is an assignment
      tupleVarDec.assignment.expressionRight shouldBe
        Identifier(
          index = indexOf("let (a, b, c) = >>blah<<"),
          text = "blah"
        )
    }
  }

}
