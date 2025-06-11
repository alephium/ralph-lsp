// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GoToEnumFieldSpec extends AnyWordSpec with Matchers {

  "return empty" when {
    "enum type does not exist" in {
      goToDefinition()(
        """
          |Contract MyContract() {
          |  pub fn function() -> () {
          |    let field = EnumType.Fie@@ld0
          |  }
          |}
          |""".stripMargin
      )
    }
  }

  "return self" when {
    "enum field definition is selected" when {
      "duplicate private enum exists" in {
        goToDefinition()(
          """
            |// This parent is not inherited
            |Abstract Contract ParentNotUsed() {
            |
            |  enum EnumType {
            |    Field0 = 0
            |    Field1 = 1
            |  }
            |}
            |
            |enum EnumType {
            |  >>Field@@0<< = 0
            |  Field1 = 1
            |}
            |""".stripMargin
        )
      }

      "second enum type has syntax error" in {
        goToDefinition()(
          """
            |enum EnumType {
            |  Field0 = 0
            |  Field1 = 1
            |}
            |
            |enum EnumType {
            |  >>Field@@0<< = 0
            |  Field1
            |}
            |""".stripMargin
        )
      }

      "no values are defined" in {
        goToDefinitionSoft()(
          """
            |enum EnumType
            |
            |enum EnumType {
            |  >>Field@@0<< =
            |}
            |""".stripMargin
        )
      }
    }
  }

  "return non-empty" when {
    "user selects the first enum field" in {
      goToDefinition()(
        """
          |// This parent is not inherited
          |Abstract Contract ParentNotUsed() {
          |
          |  enum EnumType {
          |    Field0 = 0
          |    Field1 = 1
          |  }
          |}
          |
          |enum EnumType {
          |  >>Field0<< = 0
          |  Field1 = 1
          |}
          |
          |Abstract Contract Parent() {
          |
          |  enum EnumType {
          |    >>Field0<< = 0
          |    Field1 = 1
          |  }
          |}
          |
          |Contract MyContract() extends Parent() {
          |
          |  enum EnumType {
          |    >>Field0<< = 0
          |    Field1 = 1
          |  }
          |
          |  pub fn function() -> () {
          |    let field0 = EnumType.Fie@@ld0
          |    let field1 = EnumType.Field1
          |  }
          |}
          |
          |enum EnumType {
          |  >>Field0<< = 0
          |  Field1 = 1
          |}
          |""".stripMargin
      )
    }

    "user selects the second enum field" in {
      goToDefinition()(
        """
          |enum EnumType {
          |  Field0 = 0
          |  >>Field1<< = 1
          |}
          |
          |Abstract Contract Parent() {
          |
          |  enum EnumType {
          |    Field0 = 0
          |    >>Field1<< = 1
          |  }
          |}
          |
          |
          |Contract MyContract() extends Parent() {
          |
          |  enum EnumType {
          |    Field0 = 0
          |    >>Field1<< = 1
          |  }
          |
          |  pub fn function() -> () {
          |    let field0 = EnumType.Field0
          |    let field1 = EnumType.Fie@@ld1
          |  }
          |}
          |
          |enum EnumType {
          |  Field0 = 0
          |  >>Field1<< = 1
          |}
          |""".stripMargin
      )
    }

    "there are duplicate enum types and fields" when {
      "user selects the first enum field" in {
        goToDefinition()(
          """
            |enum EnumType {
            |  >>Field0<< = 0
            |  Field1 = 1
            |}
            |
            |Abstract Contract Parent() {
            |  enum EnumType {
            |    >>Field0<< = 0
            |    Field1 = 1
            |  }
            |
            |  enum EnumType {
            |    >>Field0<< = 0
            |    Field1 = 1
            |  }
            |}
            |
            |enum EnumType {
            |  >>Field0<< = 0
            |  Field1 = 1
            |}
            |
            |Contract MyContract() extends Parent() {
            |
            |  enum EnumType {
            |    >>Field0<< = 0
            |    Field1 = 1
            |  }
            |
            |  enum EnumType {
            |    >>Field0<< = 0
            |    Field1 = 1
            |  }
            |
            |  pub fn function() -> () {
            |    let field0 = EnumType.Fi@@eld0
            |    let field1 = EnumType.Field1
            |  }
            |}
            |""".stripMargin
        )
      }

      "user selects the second enum field" in {
        goToDefinition()(
          """
            |enum EnumType {
            |  Field0 = 0
            |  >>Field1<< = 1
            |}
            |
            |Contract MyContract() {
            |
            |  enum EnumType {
            |    Field0 = 0
            |    >>Field1<< = 1
            |  }
            |
            |  enum EnumType {
            |    Field0 = 0
            |    >>Field1<< = 1
            |  }
            |
            |  pub fn function() -> () {
            |    let field0 = EnumType.Field0
            |    let field1 = EnumType.Fi@@eld1
            |  }
            |}
            |""".stripMargin
        )
      }
    }

    "there are duplicate enum types with distinct fields" when {
      "user selects the first enum field" in {
        goToDefinition()(
          """
            |enum EnumType {
            |  >>Field0<< = 0
            |  Field1 = 1
            |}
            |
            |Contract MyContract() {
            |
            |  enum EnumType {
            |    >>Field0<< = 0
            |    Field1 = 1
            |  }
            |
            |  enum EnumType {
            |    Field2 = 2
            |    Field3 = 3
            |  }
            |
            |  pub fn function() -> () {
            |    let field0 = EnumType.Fie@@ld0
            |    let field1 = EnumType.Field1
            |    let field2 = EnumType.Field2
            |    let field3 = EnumType.Field3
            |  }
            |}
            |""".stripMargin
        )
      }

      "user selects the third enum field" in {
        goToDefinition()(
          """
            |Contract MyContract() {
            |
            |  enum EnumType {
            |    Field0 = 0
            |    Field1 = 1
            |  }
            |
            |  enum EnumType {
            |    >>Field2<< = 2
            |    Field3 = 3
            |  }
            |
            |  pub fn function() -> () {
            |    let field0 = EnumType.Field0
            |    let field1 = EnumType.Field1
            |    let field2 = EnumType.Fie@@ld2
            |    let field3 = EnumType.Field3
            |  }
            |}
            |""".stripMargin
        )
      }

      "an enum field is selected that's implemented within a parent" in {
        goToDefinition()(
          """
            |Abstract Contract Parent2() {
            |  enum EnumType {
            |    >>Field0<< = 00
            |    Field3 = 3
            |  }
            |}
            |
            |Abstract Contract Parent1() extends Parent2() {
            |  enum EnumType {
            |    >>Field0<< = 00
            |    Field3 = 3
            |  }
            |}
            |
            |Contract MyContract() extends Parent1() {
            |
            |  enum EnumType {
            |    >>Field0<< = 0
            |    Field1 = 1
            |  }
            |
            |  pub fn function() -> () {
            |    let field0 = EnumType.Fiel@@d0
            |  }
            |}
            |""".stripMargin
        )
      }
    }
  }

  "SoftAST: Static calls" when {

    /**
     * [[org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST]] tests access
     * to any type definition values as static calls.
     *
     * Both the following are static calls.
     *
     * {{{
     *   MyEnum.Value
     *   MyContract.encodeFields!()
     * }}}
     *
     * But this applies not only to values, but also functions.
     */
    "single enum value is accessed" in {
      goToDefinitionSoft() {
        """
          |enum MyEnum {
          |  >>One<< = 1
          |}
          |
          |MyEnum.On@@e
          |""".stripMargin
      }
    }

    "duplicate enum value is accessed" in {
      goToDefinitionSoft() {
        """
          |enum MyEnum {
          |  >>One<< = 1
          |  >>One<< = 11
          |}
          |
          |MyEnum.On@@e
          |""".stripMargin
      }
    }

    "enum is accessed as a reference call, but no function exists" in {
      goToDefinitionSoft() {
        """
          |enum MyEnum {
          |  >>One<< = 1
          |  >>One<< = 11
          |}
          |
          |MyEnum.On@@e()
          |""".stripMargin
      }
    }

    "a method is invoked an enum value" in {
      goToDefinitionSoft() {
        """
          |enum MyEnum {
          |  >>One<< = 1
          |  >>One<< = 11
          |}
          |
          |MyEnum.On@@e.toI256
          |""".stripMargin
      }
    }
  }

}
