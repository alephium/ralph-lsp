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
      val assigment =
        parseVariableDeclaration("let mut variable = 1")

      assigment shouldBe
        SoftAST.VariableDeclaration(
          index = indexOf(">>let mut variable = 1<<"),
          let = Let(indexOf(">>let<< mut variable = 1")),
          postLetSpace = Some(SpaceOne(indexOf("let>> <<mut variable = 1"))),
          assignment = SoftAST.Assignment(
            index = indexOf("let >>mut variable = 1<<"),
            expressionLeft = SoftAST.MutableBinding(
              index = indexOf("let >>mut variable<< = 1"),
              mut = Mut(indexOf("let >>mut<< variable = 1")),
              space = Some(SpaceOne(indexOf("let mut>> <<variable = 1"))),
              identifier = Identifier(indexOf("let mut >>variable<< = 1"), "variable")
            ),
            postIdentifierSpace = Some(SpaceOne(indexOf("let mut variable>> <<= 1"))),
            equalToken = Equal(indexOf("let mut variable >>=<< 1")),
            postEqualSpace = Some(SpaceOne(indexOf("let mut variable =>> <<1"))),
            expressionRight = Number(indexOf("let mut variable = >>1<<"), "1")
          )
        )
    }
  }

  "let" should {
    "always result in variable-declaration" when {
      "right expression is missing" in {
        val assigment =
          parseVariableDeclaration("let mut variable = ")

        assigment shouldBe
          SoftAST.VariableDeclaration(
            index = indexOf(">>let mut variable = <<"),
            let = Let(indexOf(">>let<< mut variable = ")),
            postLetSpace = Some(SpaceOne(indexOf("let>> <<mut variable = "))),
            assignment = SoftAST.Assignment(
              index = indexOf("let >>mut variable = <<"),
              expressionLeft = SoftAST.MutableBinding(
                index = indexOf("let >>mut variable<< = "),
                mut = Mut(indexOf("let >>mut<< variable = ")),
                space = Some(SpaceOne(indexOf("let mut>> <<variable = "))),
                identifier = Identifier(indexOf("let mut >>variable<< = "), "variable")
              ),
              postIdentifierSpace = Some(SpaceOne(indexOf("let mut variable>> <<= "))),
              equalToken = Equal(indexOf("let mut variable >>=<< ")),
              postEqualSpace = Some(SpaceOne(indexOf("let mut variable =>> <<"))),
              expressionRight = SoftAST.ExpressionExpected(indexOf("let mut variable = >><<"))
            )
          )
      }
    }

    "equal is missing" in {
      val assigment =
        parseVariableDeclaration("let mut variable")

      assigment shouldBe
        SoftAST.VariableDeclaration(
          index = indexOf(">>let mut variable<<"),
          let = Let(indexOf(">>let<< mut variable")),
          postLetSpace = Some(SpaceOne(indexOf("let>> <<mut variable"))),
          assignment = SoftAST.Assignment(
            index = indexOf("let >>mut variable<<"),
            expressionLeft = SoftAST.MutableBinding(
              index = indexOf("let >>mut variable<<"),
              mut = Mut(indexOf("let >>mut<< variable")),
              space = Some(SpaceOne(indexOf("let mut>> <<variable"))),
              identifier = Identifier(indexOf("let mut >>variable<<"), "variable")
            ),
            postIdentifierSpace = None,
            equalToken = SoftAST.TokenExpected(indexOf("let mut variable>><<"), Token.Equal),
            postEqualSpace = None,
            expressionRight = SoftAST.ExpressionExpected(indexOf("let mut variable>><<"))
          )
        )
    }

    "variable name is missing" in {
      val varDec =
        parseVariableDeclaration("let mut")

      varDec shouldBe
        SoftAST.VariableDeclaration(
          index = indexOf(">>let mut<<"),
          let = Let(indexOf(">>let<< mut")),
          postLetSpace = Some(SpaceOne(indexOf("let>> <<mut"))),
          assignment = SoftAST.Assignment(
            index = indexOf("let >>mut<<"),
            expressionLeft = SoftAST.MutableBinding(
              index = indexOf("let >>mut<<"),
              mut = Mut(indexOf("let >>mut<<")),
              space = None,
              identifier = SoftAST.IdentifierExpected(indexOf("let mut>><<"))
            ),
            postIdentifierSpace = None,
            equalToken = SoftAST.TokenExpected(indexOf("let mut>><<"), Token.Equal),
            postEqualSpace = None,
            expressionRight = SoftAST.ExpressionExpected(indexOf("let mut>><<"))
          )
        )
    }

    "only let is defined" in {
      val assignment =
        parseVariableDeclaration("let")

      assignment shouldBe
        SoftAST.VariableDeclaration(
          index = indexOf(">>let<<"),
          let = Let(indexOf(">>let<<")),
          postLetSpace = None,
          assignment = SoftAST.Assignment(
            index = indexOf("let>><<"),
            expressionLeft = SoftAST.ExpressionExpected(indexOf("let>><<")),
            postIdentifierSpace = None,
            equalToken = SoftAST.TokenExpected(indexOf("let>><<"), Token.Equal),
            postEqualSpace = None,
            expressionRight = SoftAST.ExpressionExpected(indexOf("let>><<"))
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
      val tupleDecl =
        parseVariableDeclaration("let (a, b, c) = blah")

      tupleDecl.let shouldBe Let(indexOf(">>let<< (a, b, c) = blah"))
      tupleDecl.postLetSpace shouldBe Some(SpaceOne(indexOf("let>> <<(a, b, c) = blah")))

      // left is a tuple
      val left = tupleDecl.assignment.expressionLeft.asInstanceOf[SoftAST.Group[Token.OpenParen.type, Token.CloseParen.type]]
      left.index shouldBe indexOf("let >>(a, b, c)<< = blah")
      left.toCode() shouldBe "(a, b, c)"

      // right is an assignment
      tupleDecl.assignment.expressionRight shouldBe
        Identifier(
          index = indexOf("let (a, b, c) = >>blah<<"),
          text = "blah"
        )
    }
  }

}
