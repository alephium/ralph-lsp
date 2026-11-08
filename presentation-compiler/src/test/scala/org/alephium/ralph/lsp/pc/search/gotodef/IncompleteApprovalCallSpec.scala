// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class IncompleteApprovalCallSpec extends AnyWordSpec with Matchers {

  "Incomplete asset approval" should {
    "prefer a function to a same-named struct before parentheses are typed" in {
      goToDefinition() {
        """struct blah {}
          |fn >>blah<<() -> () {
          |  bla@@h{user}
          |}
          |""".stripMargin
      }
    }

    "resolve the callable as an approval arrow and amount are typed" in {
      goToDefinition() {
        """struct blah {}
          |fn >>blah<<() -> () { bla@@h{user -> ALPH: 1} }
          |""".stripMargin
      }
    }

    "resolve a function inherited from a parent" in {
      goToDefinition() {
        """struct blah {}
          |Abstract Contract Parent() { fn >>blah<<() -> () {} }
          |Contract Child() extends Parent() {
          |  fn run() -> () { bla@@h{user} }
          |}
          |""".stripMargin
      }
    }

    "keep resolving the function when parentheses are added" in {
      goToDefinition() {
        """struct blah {}
          |fn >>blah<<() -> () { bla@@h{user}() }
          |""".stripMargin
      }
    }

    "fall back to an available lowercase struct when there is no callable" in {
      goToDefinition() {
        """struct >>blah<< { user: Address }
          |fn run() -> () { bla@@h{user} }
          |""".stripMargin
      }
    }

    "retain explicit struct field syntax even if a function has the same name" in {
      goToDefinition() {
        """struct >>blah<< { user: Address }
          |fn blah() -> () { bla@@h{user: owner} }
          |""".stripMargin
      }
    }

    "retain uppercase struct and empty-brace syntax" in {
      goToDefinition() {
        """struct >>Blah<< { user: Address }
          |fn Blah() -> () { Bla@@h{user} }
          |""".stripMargin
      }
      goToDefinition() {
        """struct >>blah<< {}
          |fn blah() -> () { bla@@h{} }
          |""".stripMargin
      }
    }
  }

}
