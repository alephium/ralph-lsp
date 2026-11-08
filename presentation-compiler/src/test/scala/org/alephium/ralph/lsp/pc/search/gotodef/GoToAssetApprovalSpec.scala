// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.wordspec.AnyWordSpec

class GoToAssetApprovalSpec extends AnyWordSpec {

  "asset approval definitions" should {
    Seq("", "(", "()").foreach {
      arguments =>
        s"resolve both arrow sides with trailing arguments '$arguments'" in {
          goToDefBuiltIn(
            code = s"""Contract Test() {
                      |  fn test() -> () { assert!{a@@ssert!() -> assert!()}$arguments }
                      |}""".stripMargin,
            expected = Some(">>assert!<<")
          )
          goToDefBuiltIn(
            code = s"""Contract Test() {
                      |  fn test() -> () { assert!{assert!() -> a@@ssert!()}$arguments }
                      |}""".stripMargin,
            expected = Some(">>assert!<<")
          )
        }
    }

    "resolve a local address before an unfinished call" in {
      goToDefinition() {
        """Contract Test(>>address<<: Address) {
          |  fn test() -> () {
          |    transferToken!{add@@ress -> ALPH: 1 alph}
          |  }
          |}""".stripMargin
      }
    }
  }

}
