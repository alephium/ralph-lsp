// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GoToTestDeclarationSpec extends AnyWordSpec with Matchers {

  "test operations do not resolve to identically named local declarations" in {
    Seq("test", "before", "after").foreach {
      keyword =>
        def cursor(name: String): String = if (name == keyword) name.patch(2, "@@", 0) else name
        goToDefinition() {
          s"""Contract UnitTest {
           |  let test = 1
           |  let before = 2
           |  let after = 3
           |  ${cursor("test")} "example" ${cursor("before")} Self() ${cursor("after")} Self() {
           |    let copy = $keyword
           |  }
           |}""".stripMargin
        }
    }
  }

  "reference edits include real uses but exclude test-operation keywords" in {
    Seq("test", "before", "after").foreach {
      name =>
        val cursor = name.patch(2, "@@", 0)
        goToReferencesSoft(testGoToRefSetting.copy(includeDeclaration = true)) {
          s"""Contract UnitTest {
           |  let >>$cursor<< = 1
           |  test "example" before Self() after Self() {
           |    let copy = >>$name<<
           |  }
           |}""".stripMargin
        }
    }
  }

  "renaming ordinary fields preserves colliding test operations" in {
    Seq("test", "before", "after").foreach {
      name =>
        val cursor = name.patch(2, "@@", 0)
        goToRename {
          s"""Contract UnitTest(>>$cursor<<: U256) {
           |  fn read() -> U256 { return >>$name<< }
           |  test "example" before Self(1) after Self(1) {}
           |}""".stripMargin
        }
    }
  }

  "local test-body variables do not leak to another test" in {
    goToDefinition() {
      """Contract UnitTest {
        |  test "first" { let value = 1 }
        |  test "second" { let copy = val@@ue }
        |}""".stripMargin
    }
  }

  "test keyword prefixes remain navigable identifiers" in {
    goToDefinition() {
      """Contract UnitTest {
        |  let >>testing<< = 1
        |  test "example" { let copy = test@@ing }
        |}""".stripMargin
    }
  }

}
