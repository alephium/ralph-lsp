// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

/**
 * Targets test-cases that
 * - execute search on an external variable,
 * - jumping to its declaration in a struct deconstructor.
 *
 * {{{
 *   let MyStruct { >>field<< } = instance // deconstructor
 *   @@field
 * }}}
 */
class GoToStructDeconstructorField_From_VariableSpec extends AnyWordSpec with Matchers {

  "no fields are copied (no rereference created with colon)" when {
    "one field" when {
      "no duplicate variables" in {
        goToDefinition() {
          """
            |let Struct { >>field<< } = instance
            |@@field
            |""".stripMargin
        }
      }

      "a duplicate variable exists" in {
        goToDefinition() {
          """
            |let >>field<< = 1
            |let Struct { >>field<< } = instance
            |@@field
            |""".stripMargin
        }
      }
    }

    "two fields" when {
      "first field is searched" when {
        "no duplicate variable" in {
          goToDefinition() {
            """
              |let Struct { >>field1<<, field2 } = instance
              |@@field1
              |""".stripMargin
          }
        }

        "a duplicate variable exists" in {
          goToDefinition() {
            """
              |let >>field1<< = 1
              |let Struct { >>field1<<, field2 } = instance
              |@@field1
              |""".stripMargin
          }
        }
      }

      "second field is searched" when {
        "no duplicate variable exists" in {
          goToDefinition() {
            """
              |let Struct { field1, >>field2<< } = instance
              |@@field2
              |""".stripMargin
          }
        }

        "a duplicate variable exists" in {
          goToDefinition() {
            """
              |let >>field2<< = 2
              |let Struct { field1, >>field2<< } = instance
              |@@field2
              |""".stripMargin
          }
        }
      }

      "both fields are duplicated" when {
        "no duplicate variable exists" in {
          goToDefinition() {
            """
              |let Struct { >>field<<,
              |             >>field<< } = instance
              |@@field
              |""".stripMargin
          }
        }

        "a duplicate variable exists" in {
          goToDefinition() {
            """
              |let >>field<< = 1
              |let Struct { >>field<<,
              |             >>field<< } = instance
              |@@field
              |""".stripMargin
          }
        }
      }
    }
  }

  "fields are copied (rereference are created with colon)" when {
    "one field" in {
      goToDefinition() {
        """
          |let Struct { field: >>copy<< } = instance
          |@@copy
          |""".stripMargin
      }
    }

    "two fields" when {
      "first field is copied" should {
        "jump to the copied declaration" in {
          goToDefinition() {
            """
              |let Struct { field1: >>copy<<, field2 } = instance
              |@@copy
              |""".stripMargin
          }
        }

        "not jump to the original `field1`" in {
          goToDefinition() {
            """
              |let Struct { field1: copy, field2 } = instance
              |
              |// field1 is no longer a local variable, its reference is copied to `copy`
              |f@@ield1
              |""".stripMargin
          }
        }
      }

      "second field is copied" should {
        "jump to the copied declaration" in {
          goToDefinition() {
            """
              |let Struct { field1, field2: >>copy<< } = instance
              |@@copy
              |""".stripMargin
          }
        }

        "not jump to the original `field1`" in {
          goToDefinition() {
            """
              |let Struct { field1, field2: copy } = instance
              |
              |// field2 is no longer a local variable, its reference is copied to `copy`
              |f@@ield2
              |""".stripMargin
          }
        }
      }

      "all fields are copied" should {
        "jump to the first copied declaration" in {
          goToDefinition() {
            """
              |let Struct { field1: >>copy1<<, field2: copy2 } = instance
              |@@copy1
              |""".stripMargin
          }
        }

        "jump to the second copied declaration" in {
          goToDefinition() {
            """
              |let Struct { field1: copy1, field2: >>copy2<< } = instance
              |@@copy2
              |""".stripMargin
          }
        }

        "not jump to the original first field" in {
          goToDefinition() {
            """
              |let Struct { field1: copy1, field2: copy2 } = instance
              |
              |// field1 is no longer a local variable, its reference is copied to `copy1`
              |f@@ield1
              |""".stripMargin
          }
        }

        "not jump to the original second field" in {
          goToDefinition() {
            """
              |let Struct { field1: copy1, field2: copy2 } = instance
              |
              |// field2 is no longer a local variable, its reference is copied to `copy2`
              |f@@ield2
              |""".stripMargin
          }
        }
      }
    }
  }

}
