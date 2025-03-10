// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.rename

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GoToRenameEventFieldSpec extends AnyWordSpec with Matchers {

  "not rename usages" when {
    "no duplicate event fields" in {
      goToRename(
        """
          |Contract Test() {
          |
          |  event Transfer(
          |    to: Address,
          |    >>a@@mount<<: U256
          |  )
          |
          |  pub fn function() -> () {
          |    // usages are not renamed
          |    emit Transfer(to, amount)
          |    emit Transfer(to, someAmount)
          |  }
          |}
          |""".stripMargin
      )
    }

    "duplicates event fields exist" in {
      goToRename(
        """
          |Contract Test() {
          |
          |  event Transfer(
          |    to: Address,
          |    >>a@@mount<<: U256,
          |    amount: U256
          |  )
          |
          |  pub fn function() -> () {
          |    // usages are not renamed
          |    emit Transfer(to, amount)
          |    emit Transfer(to, someAmount)
          |  }
          |}
          |""".stripMargin
      )
    }

    "duplicates events exist" in {
      goToRename(
        """
          |Contract Test() {
          |
          |  event Transfer(
          |    to: Address,
          |    >>a@@mount<<: U256,
          |    amount: U256
          |  )
          |
          |  event Transfer(
          |    to: Address,
          |    amount: U256,
          |    amount: U256
          |  )
          |
          |  pub fn function() -> () {
          |    // usages are not renamed
          |    emit Transfer(to, amount)
          |    emit Transfer(to, someAmount)
          |  }
          |}
          |""".stripMargin
      )
    }
  }

}
