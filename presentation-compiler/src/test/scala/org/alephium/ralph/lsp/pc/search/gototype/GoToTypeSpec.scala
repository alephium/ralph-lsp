// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gototype

import org.alephium.ralph
import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GoToTypeSpec extends AnyWordSpec with Matchers {

  "go-to variable type" when {
    "Right-hand-side: Assigned value is being search" when {
      "custom type `Parent`" when {
        "simple variable" in {
          goToType(
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
          goToType(
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
      }
    }

  }

}
