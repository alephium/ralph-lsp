// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gotoref

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GoToEnumFieldUsageSpec extends AnyWordSpec with Matchers {

  "return empty" when {
    "there are no enum field calls or usages" when {
      "local enum" in {
        goToReferences() {
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
        }
      }

      "global enum" in {
        goToReferences() {
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
        }
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

          goToReferencesForAll(">>Field0<<".r, ">>Fie@@ld0<<")(
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

          goToReferencesForAll(">>Field1<<".r, ">>Fie@@ld1<<")(
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

          goToReferencesForAll(">>Field1<<".r, ">>Fie@@ld1<<")(
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
