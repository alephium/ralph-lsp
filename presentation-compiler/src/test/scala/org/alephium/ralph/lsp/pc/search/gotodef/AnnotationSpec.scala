// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class AnnotationSpec extends AnyWordSpec with Matchers {

  "return empty" when {
    "the annotation name has another definition with the same name" when {

      /**
       * GROUP: Duplicate is a function
       */
      "function" when {
        "without syntax errors" in {
          goToDefinition() {
            """
              |{
              |  @annotati@@on
              |  fn annotation() -> () {}
              |}
              |""".stripMargin
          }
        }

        "with syntax errors" in {
          goToDefinition() {
            """
              |{
              |  @annotati@@on(
              |  fn annotation
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
              |  @annotati@@on(property = value)
              |  let annotation = 1
              |}
              |""".stripMargin
          }
        }

        "with syntax errors" in {
          goToDefinition() {
            """
              |{
              |  @annotat@@ion(property = )
              |  let annotation =
              |}
              |""".stripMargin
          }
        }
      }
    }
  }

}
