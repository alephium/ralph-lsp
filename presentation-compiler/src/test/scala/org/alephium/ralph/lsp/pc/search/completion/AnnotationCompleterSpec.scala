// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.completion

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.alephium.ralph.lsp.pc.search.TestCodeProvider._

class AnnotationCompleterSpec extends AnyWordSpec with Matchers {

  "suggest `using` annotation name" in {
    val suggestions =
      suggest {
        """
          |Contract Test() {
          |
          |  @using@@
          |  fn function() -> () { }
          |}
          |""".stripMargin
      }

    suggestions should contain allElementsOf AnnotationCompleter.suggestAnnotationNames().toList
  }

  "suggest `inline` annotation name" in {
    val suggestions =
      suggest {
        """
          |Contract Test() {
          |
          |  @inline@@
          |  fn function() -> () { }
          |}
          |""".stripMargin
      }

    suggestions should contain allElementsOf AnnotationCompleter.suggestAnnotationNames().toList
  }

  "suggest `using` annotation keys" in {
    val suggestions =
      suggest {
        """
          |Contract Test() {
          |
          |  @using(update@@Fields = true)
          |  fn function() -> () { }
          |}
          |""".stripMargin
      }

    suggestions should contain allElementsOf AnnotationCompleter.suggestAnnotationKeys().toList
  }

  "suggest boolean for `using` annotation values" in {
    val suggestions =
      suggest {
        """
          |Contract Test() {
          |
          |  @using(updateFields = tr@@ue)
          |  fn function() -> () { }
          |}
          |""".stripMargin
      }

    suggestions should contain allElementsOf AnnotationCompleter.suggestAnnotationValues().toList
  }

}
