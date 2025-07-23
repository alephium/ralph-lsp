// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class AllowLeftShiftSpec extends AnyWordSpec with Matchers {

  "allow searching" when {
    "the last character" when {
      "is followed by space" in {
        goToDefinitionSoft() {
          """
            |struct >>MyStruct<< { }
            |
            |(MyStruct@@ )
            |""".stripMargin
        }
      }

      "is followed by another token" in {
        goToDefinitionSoft() {
          """
            |struct >>MyStruct<< { }
            |
            |(MyStruct@@)
            |""".stripMargin
        }
      }
    }

    "the first character" when {
      "is followed by space" in {
        goToDefinitionSoft() {
          """
            |struct >>MyStruct<< { }
            |
            |( @@MyStruct )
            |""".stripMargin
        }
      }

      "is followed by another token" in {
        goToDefinitionSoft() {
          """
            |struct >>MyStruct<< { }
            |
            |(@@MyStruct)
            |""".stripMargin
        }
      }
    }
  }

  "disallow searching" when {
    "the shift results in multiple spaces" when {
      "a" in {
        goToDefinitionSoft() {
          """
            |struct MyStruct { }
            |
            |(MyStruct @@)
            |""".stripMargin
        }
      }

      "b" in {
        goToDefinitionSoft() {
          """
            |struct MyStruct { }
            |
            |(MyStruct @@ )
            |""".stripMargin
        }
      }

      "c" in {
        goToDefinitionSoft() {
          """
            |struct MyStruct { }
            |
            |(MyStruct  @@ )
            |""".stripMargin
        }
      }
    }

    "the shift results in multiple tokens" when {
      "a" in {
        goToDefinitionSoft() {
          """
            |struct MyStruct { }
            |
            |((MyStruct)@@)
            |""".stripMargin
        }
      }

      "b" in {
        goToDefinitionSoft() {
          """
            |struct MyStruct { }
            |
            |((ABC, D, MyStruct,@@))
            |""".stripMargin
        }
      }

      "c" in {
        goToDefinitionSoft() {
          """
            |struct MyStruct { }
            |
            |((ABC, D, MyStruct,@@))
            |""".stripMargin
        }
      }
    }

  }

}
