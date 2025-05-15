// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gototypedef

import org.alephium.ralph
import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.alephium.ralph.Type
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GoToTypeDefinitionSpec extends AnyWordSpec with Matchers {

  "go-to variable type" when {

    /**
     * ----------------------------------------------------------------------------
     * Test cases for when the right-hand-side i.e. the variable value is searched.
     * ----------------------------------------------------------------------------
     */

    "Right-hand-side: Assigned value is being search" when {
      "custom type `Parent`" when {
        "simple variable" in {
          goToTypeDef(
            """
              |Abstract Contract >>Parent<<() { }
              |
              |Contract Child() {
              |  fn main(parent: Parent) -> () {
              |    let copy1 = paren@@t
              |  }
              |}
              |""".stripMargin
          )
        }

        "copied variables" in {
          goToTypeDef(
            """
              |Abstract Contract >>Parent<<() { }
              |
              |Contract Child() {
              |  fn main(parent: Parent) -> () {
              |    let copy1 = parent
              |    let copy2 = copy1
              |    let copy3 = copy2
              |    let copy4 = cop@@y3
              |  }
              |}
              |""".stripMargin
          )
        }
      }

      "variables are defined in built-in library" when {
        def doTest(primitiveType: ralph.Type) =
          goToTypeDefBuiltIn(Some(s"Abstract Contract >>${primitiveType.signature}<<"))(
            s"""
               |Contract Child() {
               |  fn main(primitive: ${primitiveType.signature}) -> () {
               |    let copy1 = primiti@@ve
               |  }
               |}
               |""".stripMargin
          )

        "primitives are searched" in {
          ralph.Type.primitives foreach doTest
        }

        "panic is searched" ignore {
          // TODO: Currently the type `Panic` is available in the built-in library.
          doTest(ralph.Type.Panic)
        }
      }
    }

    /**
     * --------------------------------------------------------------------------------
     * Test cases for when the left-hand-side i.e. the variable definition is searched.
     * --------------------------------------------------------------------------------
     */
    "Left-hand-side: Variable definition is being search" when {
      "custom type `Parent`" when {
        "simple variable" in {
          goToTypeDef(
            """
              |Abstract Contract >>Parent<<() { }
              |
              |Contract Child() {
              |  fn main(parent: Parent) -> () {
              |    let cop@@y1 = parent
              |  }
              |}
              |""".stripMargin
          )
        }

        "copied variables" in {
          goToTypeDef(
            """
              |Abstract Contract >>Parent<<() { }
              |
              |Contract Child() {
              |  fn main(parent: Parent) -> () {
              |    let copy1 = parent
              |    let copy2 = copy1
              |    let copy3 = copy2
              |    let cop@@y4 = copy3
              |  }
              |}
              |""".stripMargin
          )
        }
      }

      "variables are defined in built-in library" when {
        def doTest(primitiveType: ralph.Type) =
          goToTypeDefBuiltIn(Some(s"Abstract Contract >>${primitiveType.signature}<<"))(
            s"""
               |Contract Child() {
               |  fn main(primitive: ${primitiveType.signature}) -> () {
               |    let cop@@y1 = primitive
               |  }
               |}
               |""".stripMargin
          )

        "primitives are searched" in {
          ralph.Type.primitives foreach doTest
        }

        "panic is searched" ignore {
          // TODO: Currently the type `Panic` is available in the built-in library.
          doTest(ralph.Type.Panic)
        }
      }
    }

    "anonymous single variable" in {
      goToTypeDefBuiltIn(Some(s"Abstract Contract >>${Type.U256.productPrefix}<<"))(
        """
          |Contract Test() {
          |  fn test() -> () {
          |    let _@@ = 1
          |  }
          |}
          |""".stripMargin
      )
    }

    "tuple variables" when {
      "first element is selected" in {
        goToTypeDefBuiltIn(Some(s"Abstract Contract >>${Type.U256.productPrefix}<<"))(
          """
            |Contract Test() {
            |  fn foo() -> (U256, Bool) {
            |    return 1, false
            |  }
            |
            |  fn test() -> () {
            |    let (firs@@t, mut second) = foo()
            |    second = true
            |  }
            |}
            |""".stripMargin
        )
      }

      "second element is selected" in {
        goToTypeDefBuiltIn(Some(s"Abstract Contract >>${Type.Bool.productPrefix}<<"))(
          """
            |Contract Test() {
            |  fn foo() -> (U256, Bool) {
            |    return 1, false
            |  }
            |
            |  fn test() -> () {
            |    let (first, mut seco@@nd) = foo()
            |    second = true
            |  }
            |}
            |""".stripMargin
        )
      }

      "anonymous var" when {
        "the first element is anonymous" in {
          goToTypeDefBuiltIn(Some(s"Abstract Contract >>${Type.U256.productPrefix}<<"))(
            """
              |Contract Test() {
              |  fn tuple() -> (U256, Bool) {
              |    return 1, true
              |  }
              |
              |  fn test() -> () {
              |    let (_@@, mut second) = tuple()
              |    second = true
              |  }
              |}
              |""".stripMargin
          )
        }

        "the second element is anonymous" in {
          goToTypeDefBuiltIn(Some(s"Abstract Contract >>${Type.Bool.productPrefix}<<"))(
            """
              |Contract Test() {
              |  fn tuple() -> (U256, Bool) {
              |    return 1, true
              |  }
              |
              |  fn test() -> () {
              |    let (first, _@@) = tuple()
              |  }
              |}
              |""".stripMargin
          )
        }

        "both elements are anonymous" when {
          "first element is searched" in {
            goToTypeDefBuiltIn(Some(s"Abstract Contract >>${Type.U256.productPrefix}<<"))(
              """
                |Contract Test() {
                |  fn tuple() -> (U256, Bool) {
                |    return 1, true
                |  }
                |
                |  fn test() -> () {
                |    let (_@@, _) = tuple()
                |  }
                |}
                |""".stripMargin
            )
          }

          "second element is searched" in {
            goToTypeDefBuiltIn(Some(s"Abstract Contract >>${Type.Bool.productPrefix}<<"))(
              """
                |Contract Test() {
                |  fn tuple() -> (U256, Bool) {
                |    return 1, true
                |  }
                |
                |  fn test() -> () {
                |    let (_, _@@) = tuple()
                |  }
                |}
                |""".stripMargin
            )
          }
        }
      }
    }
  }

}
