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
import org.alephium.ralph.lsp.utils.Node
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class TemplateInheritanceSpec extends AnyWordSpec with Matchers {

  "fail" when {
    "implements contains typo" in {
      val template =
        parseTemplate("Contract Child() implem Parent(arg) {}")

      // implem is marked as unresolved
      template
        .toNode()
        .walkDown
        .collectFirst {
          case Node(ast @ SoftAST.Unresolved("implem", index), _) if index == indexOf("Contract Child() >>implem<< Parent(arg) {}") =>
            ast
        }
        .shouldBe(defined)
    }

    "identifier or reference is missing" in {
      val template =
        parseTemplate("Contract Child() implements {}")

      val expected =
        SoftAST.TemplateInheritance(
          index = indexOf("Contract Child() >>implements <<{}"),
          inheritanceType = SoftAST.Implements(indexOf("Contract Child() >>implements<< {}")),
          preConstructorCallSpace = SoftAST.Space(" ", indexOf("Contract Child() implements>> <<{}")),
          reference = SoftAST.IdentifierExpected(indexOf("Contract Child() implements >><<{}")),
          postConstructorCallSpace = None
        )

      template.inheritance should have size 1
      template.inheritance.head shouldBe expected
    }
  }

  "succeed" when {
    "the contract implements" when {
      "an identifier" in {
        val template =
          parseTemplate("Contract Child() implements Parent")

        val expected =
          SoftAST.TemplateInheritance(
            index = indexOf(s"Contract Child() >>implements Parent<<"),
            inheritanceType = SoftAST.Implements(indexOf(s"Contract Child() >>implements<< Parent")),
            preConstructorCallSpace = SoftAST.Space(" ", indexOf(s"Contract Child() implements>> <<Parent")),
            reference = SoftAST.Identifier("Parent", indexOf("Contract Child() implements >>Parent<<")),
            postConstructorCallSpace = None
          )

        template.inheritance should have size 1
        template.inheritance.head shouldBe expected
      }

      "a reference" in {
        val template =
          parseTemplate("Contract Child() implements Parent(arg)")

        val expected =
          SoftAST.TemplateInheritance(
            index = indexOf(s"Contract Child() >>implements Parent(arg)<<"),
            inheritanceType = SoftAST.Implements(indexOf(s"Contract Child() >>implements<< Parent(arg)")),
            preConstructorCallSpace = SoftAST.Space(" ", indexOf(s"Contract Child() implements>> <<Parent(arg)")),
            reference = SoftAST.ReferenceCall(
              index = indexOf(s"Contract Child() implements >>Parent(arg)<<"),
              reference = SoftAST.Identifier("Parent", indexOf(s"Contract Child() implements >>Parent<<(arg)")),
              preArgumentsSpace = None,
              arguments = SoftAST.Arguments(
                index = indexOf(s"Contract Child() implements Parent>>(arg)<<"),
                openParen = SoftAST.OpenParen(indexOf(s"Contract Child() implements Parent>>(<<arg)")),
                preHeadArgumentSpace = None,
                headArgument = Some(SoftAST.Argument("arg", indexOf(s"Contract Child() implements Parent(>>arg<<)"))),
                tailArguments = Seq.empty,
                closeParen = SoftAST.CloseParen(indexOf(s"Contract Child() implements Parent(arg>>)<<"))
              )
            ),
            postConstructorCallSpace = None
          )

        template.inheritance should have size 1
        template.inheritance.head shouldBe expected
      }
    }

    "the contract extends" when {
      "an identifier" in {
        val template =
          parseTemplate("Contract Child() extends Parent")

        val expected =
          SoftAST.TemplateInheritance(
            index = indexOf(s"Contract Child() >>extends Parent<<"),
            inheritanceType = SoftAST.Extends(indexOf(s"Contract Child() >>extends<< Parent")),
            preConstructorCallSpace = SoftAST.Space(" ", indexOf(s"Contract Child() extends>> <<Parent")),
            reference = SoftAST.Identifier("Parent", indexOf("Contract Child() extends >>Parent<<")),
            postConstructorCallSpace = None
          )

        template.inheritance should have size 1
        template.inheritance.head shouldBe expected
      }

      "a reference" in {
        val template =
          parseTemplate("Contract Child() extends Parent(arg)")

        val expected =
          SoftAST.TemplateInheritance(
            index = indexOf(s"Contract Child() >>extends Parent(arg)<<"),
            inheritanceType = SoftAST.Extends(indexOf(s"Contract Child() >>extends<< Parent(arg)")),
            preConstructorCallSpace = SoftAST.Space(" ", indexOf(s"Contract Child() extends>> <<Parent(arg)")),
            reference = SoftAST.ReferenceCall(
              index = indexOf(s"Contract Child() extends >>Parent(arg)<<"),
              reference = SoftAST.Identifier("Parent", indexOf(s"Contract Child() extends >>Parent<<(arg)")),
              preArgumentsSpace = None,
              arguments = SoftAST.Arguments(
                index = indexOf(s"Contract Child() extends Parent>>(arg)<<"),
                openParen = SoftAST.OpenParen(indexOf(s"Contract Child() extends Parent>>(<<arg)")),
                preHeadArgumentSpace = None,
                headArgument = Some(SoftAST.Argument("arg", indexOf(s"Contract Child() extends Parent(>>arg<<)"))),
                tailArguments = Seq.empty,
                closeParen = SoftAST.CloseParen(indexOf(s"Contract Child() extends Parent(arg>>)<<"))
              )
            ),
            postConstructorCallSpace = None
          )

        template.inheritance should have size 1
        template.inheritance.head shouldBe expected
      }
    }
  }

}
