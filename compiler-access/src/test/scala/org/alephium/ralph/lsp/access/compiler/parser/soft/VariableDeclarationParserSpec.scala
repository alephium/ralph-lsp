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
      val left = tupleVarDec.assignment.expressionLeft.asInstanceOf[SoftAST.Group[Token.OpenParen.type, Token.CloseParen.type, Token.Comma.type]]
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

  "Anonymous variable declaration" in {
    val varDec =
      parseVariableDeclaration("let _ = 1")

    varDec shouldBe
      SoftAST.VariableDeclaration(
        index = indexOf(">>let _ = 1<<"),
        let = Let(">>let<< _ = 1"),
        postLetSpace = Some(Space("let>> <<_ = 1")),
        assignment = SoftAST.Assignment(
          index = indexOf("let >>_ = 1<<"),
          expressionLeft = Identifier(
            index = indexOf("let >>_<< = 1"),
            text = "_"
          ),
          postIdentifierSpace = Some(Space("let _>> <<= 1")),
          equalToken = Equal(indexOf("let _ >>=<< 1")),
          postEqualSpace = Some(Space("let _ =>> <<1")),
          expressionRight = Number("let _ = >>1<<")
        )
      )
  }

  "a tuple element is underscore" in {
    val tupleVarDec =
      parseVariableDeclaration("let (a, _) = blah")

    tupleVarDec.let shouldBe Let(">>let<< (a, _) = blah")
    tupleVarDec.postLetSpace shouldBe Some(Space("let>> <<(a, _) = blah"))

    // left is a tuple
    val left = tupleVarDec.assignment.expressionLeft.asInstanceOf[SoftAST.Group[Token.OpenParen.type, Token.CloseParen.type, Token.Comma.type]]
    left.index shouldBe indexOf("let >>(a, _)<< = blah")
    left.toCode() shouldBe "(a, _)"

    // `a` is stored as an Identifier
    left.tailExpressions should have size 1
    val aExpression = left.headExpression.value
    aExpression shouldBe Identifier("let (>>a<<, _) = blah")

    // The underscore is stored as an Identifier
    left.tailExpressions should have size 1
    val underscore = left.tailExpressions.head
    underscore.expression shouldBe Identifier("let (a, >>_<<) = blah")

    // right is an assignment
    tupleVarDec.assignment.expressionRight shouldBe
      Identifier(
        index = indexOf("let (a, _) = >>blah<<"),
        text = "blah"
      )
  }

}
