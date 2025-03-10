// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gotoref

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.alephium.ralph.lsp.pc.search.TestCodeProvider._

class GoToTypeIdStructUsageSpec extends AnyWordSpec with Matchers {

  "return empty" when {
    "no usage exists" in {
      goToReferences() {
        """
          |struct Foo@@ {
          |  x: U256,
          |  y: U256
          |}
          |""".stripMargin
      }
    }
  }

  "return non-empty" when {
    "usage exists" when {
      "within itself" in {
        goToReferencesForAll(">>Foo<<".r, ">>Fo@@o<<")(
          """
            |struct Foo@@ {
            |  x: U256,
            |  mut foo: >>Foo<<
            |}
            |""".stripMargin
        )
      }

      "within another struct" in {
        goToReferencesForAll(">>Foo<<".r, ">>Fo@@o<<")(
          """
            |struct Foo@@ { x: U256 }
            |struct Bar { mut foo: >>Foo<< }
            |""".stripMargin
        )
      }

      "within a contract" in {
        goToReferencesForAll(">>Foo<<".r, ">>Fo@@o<<")(
          """
            |struct Foo@@ { x: U256 }
            |
            |Contract Test(foo: >>Foo<<) {
            |
            |  fn function(foo: >>Foo<<) -> () {
            |    let foo = >>Foo<< { x: 1 }
            |  }
            |
            |}
            |""".stripMargin
        )
      }
    }
  }

}
