// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class AnnotationFieldSpec extends AnyWordSpec with Matchers {

  "return empty" when {
    "the annotation assignment property has a duplicate definition" when {

      /**
       * GROUP: Duplicate is a function
       */
      "function" when {
        "without syntax errors" in {
          goToDefinition() {
            """
              |{
              |  @annotation(propert@@y = value)
              |  fn property() -> () {}
              |}
              |""".stripMargin
          }
        }

        "with syntax errors" in {
          goToDefinition() {
            """
              |{
              |  @annotation(propert@@y = )
              |  fn property
              |}
              |""".stripMargin
          }
        }
      }

      /**
       * GROUP: Duplicate is a variable
       */
      "variable" when {
        "without syntax errors" in {
          goToDefinition() {
            """
              |{
              |  @annotation(propert@@y = value)
              |  let property = 1
              |}
              |""".stripMargin
          }
        }

        "with syntax errors" in {
          goToDefinition() {
            """
              |{
              |  @annotation(propert@@y = )
              |  let property =
              |}
              |""".stripMargin
          }
        }
      }
    }
  }

}
