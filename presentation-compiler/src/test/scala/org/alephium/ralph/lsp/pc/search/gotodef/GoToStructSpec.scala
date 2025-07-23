// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GoToStructSpec extends AnyWordSpec with Matchers {

  "return self" when {
    "strict-parseable" when {
      "struct exists" in {
        goToDefinitionSoft() {
          """
            |struct >>MyStruc@@t<< {
            |  structField: Bool
            |}
            |""".stripMargin
        }
      }

      "duplicate structs exist" in {
        goToDefinitionSoft() {
          """
            |{
            |  struct MyStruct {
            |    structField: Bool
            |  }
            |
            |  struct >>MyStruc@@t<< {
            |    structField: Bool
            |  }
            |}
            |""".stripMargin
        }
      }
    }

    "soft-parseable" when {
      "struct exists" in {
        goToDefinitionSoft() {
          """
            |struct >>MyStruc@@t<<
            |""".stripMargin
        }
      }

      "duplicate structs exist" in {
        goToDefinitionSoft() {
          """
            |{
            |  struct MyStruct
            |  struct >>MyStruc@@t<<
            |}
            |""".stripMargin
        }
      }
    }
  }

  "struct is accessed" when {
    "from contract parameter" in {
      goToDefinitionSoft() {
        """
          |struct >>MyStruct<< { }
          |
          |Contract Test(struct: MyStru@@ct) { }
          |""".stripMargin
      }
    }

    "from function parameter" when {
      "global" in {
        goToDefinitionSoft() {
          """
            |struct >>MyStruct<< { }
            |
            |Contract Test {
            |
            |  fn main(struct: MyStru@@ct)
            |
            |}
            |""".stripMargin
        }
      }

      "local" in {
        goToDefinitionSoft() {
          """
            |
            |Contract Test {
            |
            |  struct >>MyStruct<< { }
            |
            |  fn main(struct: MyStru@@ct)
            |
            |}
            |""".stripMargin
        }
      }
    }

    "from local variable" when {
      "global" in {
        goToDefinitionSoft() {
          """
            |struct >>MyStruct<< { }
            |
            |Contract Test {
            |
            |  fn main() {
            |    MyStru@@ct {}
            |  }
            |
            |}
            |""".stripMargin
        }
      }

      "local" in {
        goToDefinitionSoft() {
          """
            |Contract Test {
            |
            |  fn main() {
            |    MyStru@@ct {}
            |  }
            |
            | struct >>MyStruct<< { }
            |
            |}
            |""".stripMargin
        }
      }
    }
  }

  "duplicate structs" in {
    goToDefinitionSoft() {
      """
        |struct >>MyStruct<< { }
        |
        |Contract Test {
        |
        |  struct >>MyStruct<< { }
        |
        |  fn main() -> () {
        |    MyStru@@ct { }
        |  }
        |
        | struct >>MyStruct<< { }
        |}
        |
        |struct >>MyStruct<< { }
        |""".stripMargin
    }
  }

  "nested structs" when {
    "single" in {
      goToDefinitionSoft() {
        """
          |struct >>Node<< {
          |  child: No@@de
          |}
          |""".stripMargin
      }
    }

    "duplicated" in {
      goToDefinitionSoft() {
        """
          |struct >>Node<< {}
          |
          |struct >>Node<< {
          |  child: No@@de
          |}
          |
          |struct >>Node<< {}
          |""".stripMargin
      }
    }
  }

}
