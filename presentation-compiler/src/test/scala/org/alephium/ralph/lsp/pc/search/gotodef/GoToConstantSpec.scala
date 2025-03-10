// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GoToConstantSpec extends AnyWordSpec with Matchers {

  "return empty" when {
    "constant does not exist" in {
      goToDefinitionStrict()(
        """
          |Contract GoToConstant() {
          |
          |  pub fn function() -> () {
          |    let my_constant = MyCo@@nstant
          |  }
          |}
          |""".stripMargin
      )
    }
  }

  "return self" when {
    "constant definition is selected" in {
      goToDefinitionStrict()(
        """
          |Contract Test() {
          |
          |  const >>My@@Constant<< = 1
          |
          |  pub fn function() -> () { }
          |}
          |""".stripMargin
      )
    }

    "duplicate constant definitions exist" in {
      goToDefinitionStrict()(
        """
          |Contract Test() {
          |
          |  const MyConstant = 1
          |  const >>My@@Constant<< = 1
          |
          |  pub fn function() -> () { }
          |}
          |""".stripMargin
      )
    }
  }

  "return non-empty" when {
    "constant exists" in {
      goToDefinitionStrict()(
        """
          |const >>MyConstant<< = 1
          |
          |Abstract Contract Parent() {
          |  const >>MyConstant<< = 1
          |}
          |
          |Contract Child() extends Parent() {
          |
          |  const >>MyConstant<< = 0
          |
          |  pub fn function() -> () {
          |    let my_constant = MyCo@@nstant
          |  }
          |}
          |""".stripMargin
      )
    }

    "duplicate constants exists" in {
      goToDefinitionStrict()(
        """
          |const >>MyConstant<< = 0
          |const >>MyConstant<< = 1
          |const >>MyConstant<< = 2
          |const >>MyConstant<< = 3
          |
          |Abstract Contract Parent() {
          |  const >>MyConstant<< = 2
          |  const >>MyConstant<< = 3
          |}
          |
          |Contract Child() extends Parent() {
          |
          |  const >>MyConstant<< = 0
          |  const >>MyConstant<< = 1
          |
          |  pub fn function() -> () {
          |    let my_constant = MyCo@@nstant
          |  }
          |}
          |""".stripMargin
      )
    }

    "constant and the Contract have the same name" in {
      goToDefinitionStrict()(
        """
          |Abstract Contract MyConstant() {
          |
          |  const >>MyConstant<< = 2
          |  const >>MyConstant<< = 3
          |}
          |
          |Contract MyConstant() extends MyConstant() {
          |
          |  const >>MyConstant<< = 0
          |  const >>MyConstant<< = 1
          |
          |  pub fn function() -> () {
          |    let my_constant = MyCo@@nstant
          |  }
          |}
          |""".stripMargin
      )
    }

    "only a global constant exists" in {
      goToDefinitionStrict()(
        """
          |const >>MyConstant<< = 0
          |
          |Contract Test() {
          |
          |  pub fn function() -> () {
          |    let my_constant = MyCo@@nstant
          |  }
          |}
          |""".stripMargin
      )
    }

    "constants with expression" in {
      goToDefinitionStrict() {
        """
          |const ONE = 1
          |const TWO = 2
          |const >>THREE<< = ONE + TWO
          |
          |Contract Test() {
          |  pub fn main() -> () {
          |     let three = THRE@@E
          |  }
          |}
          |""".stripMargin
      }
    }

    "constants is defined after its usage" in {
      goToDefinitionStrict() {
        """
          |Contract Test() {
          |  pub fn main() -> () {
          |     let one = ONE@@
          |  }
          |}
          |
          |const >>ONE<< = 1
          |""".stripMargin
      }
    }

    "Issue #254: Global constant has no tail newline" in {
      // https://github.com/alephium/ralph-lsp/issues/254
      goToDefinitionStrict() {
        """
          |Contract Test() {
          |  pub fn main() -> () {
          |     let one = ONE@@
          |  }
          |}
          |
          |const >>ONE<< = 1""".stripMargin
      }
    }
  }

}
