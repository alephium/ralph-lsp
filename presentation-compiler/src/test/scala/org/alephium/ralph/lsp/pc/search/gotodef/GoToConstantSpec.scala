// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GoToConstantSpec extends AnyWordSpec with Matchers {

  "return empty" when {
    "constant does not exist" in {
      goToDefinition() {
        """
          |Contract GoToConstant() {
          |
          |  pub fn function() -> () {
          |    let my_constant = MyCo@@nstant
          |  }
          |}
          |""".stripMargin
      }
    }
  }

  "return self" when {
    "constant definition is selected" when {
      "strict-parseable" when {
        "contract is defined" in {
          goToDefinition() {
            """
              |Contract Test() {
              |
              |  const >>My@@Constant<< = 1
              |
              |}
              |""".stripMargin
          }
        }

        "contract is not defined" in {
          goToDefinition() {
            """
              |const >>My@@Constant<< = 1
              |""".stripMargin
          }
        }
      }

      "soft-parseable" when {
        "value is not defined" in {
          goToDefinitionSoft() {
            """
              |const >>My@@Constant<< =
              |""".stripMargin
          }
        }

        "assignment is not defined" in {
          goToDefinitionSoft() {
            """
              |const >>My@@Constant<<
              |""".stripMargin
          }
        }
      }
    }

    "duplicate constant definitions exist" when {
      "strict-parseable" in {
        goToDefinition() {
          """
            |Contract Test() {
            |
            |  const MyConstant = 1
            |  const >>My@@Constant<< = 1
            |  
            |}
            |""".stripMargin
        }
      }

      "soft-parseable" in {
        goToDefinitionSoft() {
          """
            |Contract Test() {
            |
            |  const MyConstant
            |  const >>My@@Constant<<
            |  
            |}
            |""".stripMargin
        }
      }
    }
  }

  "constant exists locally" when {
    "strict-parseable" in {
      goToDefinition()(
        """
          |Contract Child() {
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

    "soft-parseable" when {
      "function name is not defined" in {
        goToDefinitionSoft()(
          """
            |Contract Child() {
            |
            |  const >>MyConstant<< = 0
            |
            |  fn () -> () {
            |    let my_constant = MyCo@@nstant
            |  }
            |}
            |""".stripMargin
        )
      }

      "constant is not assigned an identifier" in {
        goToDefinitionSoft()(
          """
            |Contract Child() {
            |
            |  const >>MyConstant<< = 0
            |
            |  fn () -> () {
            |    MyCo@@nstant
            |""".stripMargin
        )
      }

      "blocks are not defined" in {
        goToDefinitionSoft()(
          """
            |const >>MyConstant<< = 0
            |MyCo@@nstant
            |""".stripMargin
        )
      }

      "block is defined for reference" in {
        goToDefinitionSoft()(
          """
            |const >>MyConstant<< = 0
            |
            |Contract block {
            |  MyCo@@nstant
            |}
            |""".stripMargin
        )
      }
    }
  }

  "constant exists within inheritance" when {
    "all constants have the same name" in {
      goToDefinition()(
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
      goToDefinition()(
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

    "soft-parseable (contains errors)" in {
      goToDefinitionSoft()(
        """
          |const >>MyConstant<<
          |blah
          |const >>MyConstant<< blah
          |const >>MyConstant<<
          |const >>MyConstant<< blah
          |
          |Contract Parent {
          |  const >>MyConstant<<
          |  const >>MyConstant<<
          |}
          |
          |Contract Child extends Parent {
          |
          |  const >>MyConstant<<
          |  const >>MyConstant<<
          |
          |  fn () -> {
          |    let my_constant = MyCo@@nstant
          |""".stripMargin
      )
    }
  }

  "constant and the Contract have the same name" in {
    goToDefinition()(
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
    goToDefinition()(
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
    goToDefinition() {
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

  "constants is defined after its usage" when {
    "strict-parseable" in {
      goToDefinition() {
        """
          |Contract Test() {
          |  pub fn main() -> () {
          |     let one = ON@@E
          |  }
          |}
          |
          |const >>ONE<< = 1
          |""".stripMargin
      }
    }
  }

  "Issue #254: Global constant has no tail newline" in {
    // https://github.com/alephium/ralph-lsp/issues/254
    goToDefinition() {
      """
        |Contract Test() {
        |  pub fn main() -> () {
        |     let one = ON@@E
        |  }
        |}
        |
        |const >>ONE<< = 1""".stripMargin
    }
  }

}
