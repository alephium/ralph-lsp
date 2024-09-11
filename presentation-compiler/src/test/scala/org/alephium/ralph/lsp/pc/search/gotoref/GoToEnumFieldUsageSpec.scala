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

package org.alephium.ralph.lsp.pc.search.gotoref

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GoToEnumFieldUsageSpec extends AnyWordSpec with Matchers {

  "return empty" when {
    "there are no enum field calls or usages" when {
      "local enum" in {
        goToReferences(
          """
            |Contract MyContract() {
            |
            |  enum EnumType {
            |    Fie@@ld0 = 0
            |    Field1 = 1
            |  }
            |
            |  pub fn function() -> () { }
            |}
            |""".stripMargin
        )
      }

      "global enum" in {
        goToReferences(
          """
            |enum EnumType {
            |  Fie@@ld0 = 0
            |  Field1 = 1
            |}
            |
            |Contract MyContract() {
            |
            |  pub fn function() -> () { }
            |}
            |""".stripMargin
        )
      }
    }
  }

  "return non-empty" when {
    "there are multiple enum field calls or usages" when {
      "the first field is selected" when {
        def doTest(global: Boolean) = {
          val enumDef =
            """
              |enum EnumType {
              |  Fie@@ld0 = 0
              |  Field1 = 1
              |}
              |""".stripMargin

          goToReferences(
            s"""
              |${if (global) enumDef else ""}
              |
              |Contract MyContract() {
              |
              |  ${if (!global) enumDef else ""}
              |
              |  pub fn function() -> () {
              |    let field0 = EnumType.>>Field0<<
              |    let field1 = EnumType.Field1
              |  }
              |
              |  pub fn function() -> () {
              |    let field0 = EnumType.>>Field0<<
              |    for (let mut index = 0; index <= 4; index = index + 1) {
              |      let field1 = EnumType.Field1
              |    }
              |  }
              |}
              |""".stripMargin
          )
        }

        "local enum" in {
          doTest(global = false)
        }

        "global enum" in {
          doTest(global = true)
        }
      }

      "the second field is selected" when {
        def doTest(global: Boolean) = {
          val enumDef =
            """
              |enum EnumType {
              |  Field0 = 0
              |  Fie@@ld1 = 1
              |}
              |""".stripMargin

          goToReferences(
            s"""
               |${if (global) enumDef else ""}
               |
               |Contract MyContract() {
               |
               |  ${if (!global) enumDef else ""}
               |
               |  pub fn function() -> () {
               |    let field0 = EnumType.Field0
               |    let field1 = EnumType.>>Field1<<
               |  }
               |
               |  pub fn function() -> () {
               |    let field0 = EnumType.Field0
               |    for (let mut index = 0; index <= 4; index = index + 1) {
               |      let field1 = EnumType.>>Field1<<
               |    }
               |  }
               |}
                |""".stripMargin
          )
        }

        "local enum" in {
          doTest(global = false)
        }

        "global enum" in {
          doTest(global = true)
        }
      }

      "there is inheritance" when {
        def doTest(global: Boolean) = {
          val enumDef =
            """
              |enum EnumType {
              |  Field0 = 0
              |  Fie@@ld1 = 1
              |}
              |""".stripMargin

          goToReferences(
            s"""
               |${if (global) enumDef else ""}
               |
               |Abstract Contract Parent() {
               |
               |  ${if (!global) enumDef else ""}
               |
               |  fn function0() -> () {
               |    let field0 = EnumType.Field0
               |    let field1 = EnumType.>>Field1<<
               |  }
               |}
               |
               |Contract Parent1() extends Parent() {
               |
               |  pub fn function1() -> () {
               |    let field1 = EnumType.>>Field1<<
               |    let field0 = EnumType.Field0
               |  }
               |}
               |
               |Contract Child() extends Parent1() {
               |
               |  pub fn function2() -> () {
               |    let field1 = EnumType.>>Field1<<
               |    let field0 = EnumType.Field0
               |  }
               |}
              |""".stripMargin
          )
        }

        "local enum" in {
          doTest(global = false)
        }

        "global enum" in {
          doTest(global = true)
        }
      }
    }
  }

}
