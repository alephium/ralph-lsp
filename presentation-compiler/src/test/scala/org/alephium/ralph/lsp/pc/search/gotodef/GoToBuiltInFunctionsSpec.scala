// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.alephium.ralph.lsp.pc.workspace.build.dependency.DependencyID
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GoToBuiltInFunctionsSpec extends AnyWordSpec with Matchers {

  "return empty" when {
    "built-in does not exists" in {
      goToBuiltIn(
        code = """
            |Contract Test() {
            |  pub fn function() -> () {
            |    bla@@h!()
            |  }
            |}
            |""".stripMargin,
        expected = None
      )
    }
  }

  "return non-empty" when {
    "assert!" when {
      "native builtin library" in {
        // Expect go-to definition to work directly on the native builtin library
        goToBuiltIn(
          code = """
              |Contract Test() {
              |  pub fn function() -> () {
              |    @@assert!()
              |  }
              |}
              |""".stripMargin,
          expected = Some("""assert!""")
        )
      }

      "custom builtin library" in {
        // Expect go-to definition to work on the following custom builtin code
        goToDefinitionOnDependency(
          dependencyId = DependencyID.BuiltIn,
          // the custom builtin library
          dependency = """
              |Interface TestBuiltIn {
              |  fn hello!() -> ()
              |
              |  fn >>assert!<<() -> ()
              |
              |  fn blah!() -> ()
              |}
              |""".stripMargin,
          // the developer's workspace code
          workspace = """
              |Contract Test() {
              |  pub fn function() -> () {
              |    @@assert!()
              |  }
              |}
              |""".stripMargin
        )
      }

    }

    "verifyAbsoluteLocktime!" in {
      goToBuiltIn(
        code = """
            |Contract Test() {
            |  pub fn function() -> () {
            |    for (let mut index = 0; index <= 4; index = index + 1) {
            |      @@verifyAbsoluteLocktime!()
            |    }
            |  }
            |}
            |""".stripMargin,
        expected = Some("""verifyAbsoluteLocktime!""")
      )
    }

    "there are duplicate local and built-in function names" in {
      goToBuiltIn(
        code = """
            |Contract Test() {
            |
            |  // this function should not get selected.
            |  fn assert() -> () {
            |
            |  }
            |
            |  pub fn function() -> () {
            |    @@assert!()
            |  }
            |}
            |""".stripMargin,
        expected = Some("""assert!""")
      )
    }
  }

}
