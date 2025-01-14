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

import org.alephium.ralph.error.CompilerError
import org.alephium.ralph.lsp.access.compiler.parser.soft.TestParser._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.TestSoftAST._
import org.alephium.ralph.lsp.access.util.TestCodeUtil._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.TryValues.convertTryToSuccessOrFailure

import scala.util.Try

class VariableDeclarationSpec extends AnyWordSpec with Matchers {

  "succeed" when {
    "full valid variable declaration is defined" in {
      val assigment =
        parseVariableDeclaration("let mut variable = 1")

      assigment shouldBe
        SoftAST.VariableDeclaration(
          index = indexOf(">>let mut variable = 1<<"),
          modifiers = Seq(
            SoftAST.AssignmentAccessModifier(
              index = indexOf(">>let <<mut variable = 1"),
              token = Let(indexOf(">>let<< mut variable = 1")),
              postTokenSpace = SpaceOne(indexOf("let>> <<mut variable = 1"))
            ),
            SoftAST.AssignmentAccessModifier(
              index = indexOf("let >>mut <<variable = 1"),
              token = Mut(indexOf("let >>mut<< variable = 1")),
              postTokenSpace = SpaceOne(indexOf("let mut>> <<variable = 1"))
            )
          ),
          assignment = SoftAST.Assignment(
            index = indexOf("let mut >>variable = 1<<"),
            identifier = Identifier(indexOf("let mut >>variable<< = 1"), "variable"),
            postIdentifierSpace = Some(SpaceOne(indexOf("let mut variable>> <<= 1"))),
            equalToken = Equal(indexOf("let mut variable >>=<< 1")),
            postEqualSpace = Some(SpaceOne(indexOf("let mut variable =>> <<1"))),
            expression = Number(indexOf("let mut variable = >>1<<"), "1")
          )
        )
    }
  }

  "error" when {
    "expression is missing" in {
      val assigment =
        parseVariableDeclaration("let mut variable = ")

      assigment shouldBe
        SoftAST.VariableDeclaration(
          index = indexOf(">>let mut variable = <<"),
          modifiers = Seq(
            SoftAST.AssignmentAccessModifier(
              index = indexOf(">>let <<mut variable = "),
              token = Let(indexOf(">>let<< mut variable = ")),
              postTokenSpace = SpaceOne(indexOf("let>> <<mut variable = "))
            ),
            SoftAST.AssignmentAccessModifier(
              index = indexOf("let >>mut <<variable = "),
              token = Mut(indexOf("let >>mut<< variable = ")),
              postTokenSpace = SpaceOne(indexOf("let mut>> <<variable = "))
            )
          ),
          assignment = SoftAST.Assignment(
            index = indexOf("let mut >>variable = <<"),
            identifier = Identifier(indexOf("let mut >>variable<< = "), "variable"),
            postIdentifierSpace = Some(SpaceOne(indexOf("let mut variable>> <<= "))),
            equalToken = Equal(indexOf("let mut variable >>=<< ")),
            postEqualSpace = Some(SpaceOne(indexOf("let mut variable =>> << "))),
            expression = SoftAST.ExpressionExpected(indexOf("let mut variable = >><<"))
          )
        )
    }
  }

  "let" should {
    "not be allowed as variable name" in {
      Try(parseVariableDeclaration("let let = 1"))
        .failure
        .exception
        .getCause shouldBe a[CompilerError.FastParseError]
    }

    "allow letter as variable name" in {
      val varDec =
        parseVariableDeclaration("let letter = 1")

      varDec.assignment.identifier.code.text shouldBe "letter"
    }
  }

}
