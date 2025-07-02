// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gototypedef

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GoToTypeDefFuncIdSpec extends AnyWordSpec with Matchers {

  "return empty" when {
    "function does not exist" in {
      goToTypeDef(
        """
          |Contract Child() {
          |
          |  fn main() -> () {
          |    let copy = functi@@on()
          |  }
          |}
          |""".stripMargin
      )
    }
  }

  "local function" when {
    "function's returned value is assigned" in {
      goToTypeDef(
        """
          |Abstract Contract >>Parent<<() { }
          |
          |Contract Child(parent: Parent) {
          |
          |  fn function() -> Parent {
          |    return parent
          |  }
          |
          |  fn main() -> () {
          |    let parent = functi@@on()
          |  }
          |}
          |""".stripMargin
      )
    }

    "function's returned value is not assigned" ignore {
      // FIXME: See issue #1295 on dev alephium.
      goToTypeDef(
        """
          |Abstract Contract >>Parent<<() { }
          |
          |Contract Child(parent: Parent) {
          |
          |  fn function() -> Parent {
          |    return parent
          |  }
          |
          |  fn main() -> () {
          |    functi@@on()
          |  }
          |}
          |""".stripMargin
      )
    }
  }

  "inherited function" when {
    "function's returned value is assigned" in {
      goToTypeDef(
        """
          |Abstract Contract >>GrandParent<<() { }
          |
          |Abstract Contract Parent(grandParent: GrandParent) {
          |  fn function() -> GrandParent {
          |    return grandParent
          |  }
          |}
          |
          |Contract Child(parent: Parent) {
          |
          |  fn function() -> Parent {
          |    return parent
          |  }
          |
          |  fn main() -> () {
          |    let copy = function().functio@@n()
          |  }
          |}
          |""".stripMargin
      )
    }

    "function's returned value is not assigned" ignore {
      // FIXME: See issue #1295 on dev alephium.
      goToTypeDef(
        """
          |Abstract Contract >>GrandParent<<() { }
          |
          |Abstract Contract Parent(grandParent: GrandParent) {
          |  fn grandParent() -> GrandParent {
          |    return grandParent
          |  }
          |}
          |
          |Contract Child(parent: Parent) {
          |
          |  fn parent() -> Parent {
          |    return parent
          |  }
          |
          |  fn main() -> () {
          |    parent().grandPar@@ent()
          |  }
          |}
          |""".stripMargin
      )
    }
  }

  "nested function call returns back to `Child` contract" in {
    goToTypeDef(
      """
        |Abstract Contract GrandParent(child: Child) {
        |  fn function() -> Child {
        |    return child
        |  }
        |}
        |
        |Abstract Contract Parent(grandParent: GrandParent) {
        |  fn function() -> GrandParent {
        |    return grandParent
        |  }
        |}
        |
        |Contract >>Child<<(parent: Parent) {
        |
        |  fn function() -> Parent {
        |    return parent
        |  }
        |
        |  fn main() -> () {
        |    let copy = function().function().functi@@on()
        |  }
        |}
        |""".stripMargin
    )
  }

  "type-info of the mid nested function call is searched" when {
    "variable is assigned" in {
      goToTypeDefBuiltIn(expected = Some("Abstract Contract >>I256<<"))(
        """
          |Contract Main() {
          |  fn main() -> () {
          |    let min = i256M@@in!()
          |  }
          |}
          |""".stripMargin
      )
    }

    "variable is not assigned" ignore {
      // FIXME: See issue #1295 on dev alephium.
      goToTypeDefBuiltIn(expected = Some("Abstract Contract >>I256<<"))(
        """
          |Contract Main() {
          |  fn main() -> () {
          |    i256M@@in!()
          |  }
          |}
          |""".stripMargin
      )
    }
  }

}
