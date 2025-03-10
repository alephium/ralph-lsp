// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.rename

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.alephium.ralph.lsp.pc.search.TestCodeProvider._

class GoToRenameTypeSpec extends AnyWordSpec with Matchers {

  "rename self" when {
    "there are no references" in {
      goToRename(
        """
          |Contract >>Tes@@t<<(variable: Bool) {
          |  fn test() -> () { }
          |}
          |""".stripMargin
      )
    }
  }

  "rename all occurrences" when {
    "contract type is renamed" in {
      goToRenameForAll(">>Parent<<".r, ">>Pare@@nt<<")(
        """
          |Abstract Contract >>Paren@@t<<(variable: Bool) extends Parent2(variable) { }
          |
          |Contract Child(variable: Bool)
          |               extends >>Parent<<(variable) {
          |
          |  fn child(param: >>Parent<<) -> () {
          |
          |    while(true) {
          |      let _ = >>Parent<<.encodeFields!()
          |    }
          |
          |  }
          |
          |}
          |""".stripMargin
      )
    }

    "duplicate contracts exist" in {
      goToRenameForAll(">>Parent<<".r, ">>Pare@@nt<<")(
        """
          |Abstract Contract >>Paren@@t<<(variable: Bool) { }
          |
          |Abstract Contract >>Parent<<(variable: Bool) { }
          |""".stripMargin
      )
    }
  }

}
