// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gotoref

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GoToArgumentUsageInTxScriptSpec extends AnyWordSpec with Matchers {

  "return empty" when {
    "argument is not used" in {
      goToReferences() {
        """
          |TxScript GoToArgument(interfa@@ce: MyInterface) {
          |  let result = blah.function()
          |}
          |""".stripMargin
      }
    }
  }

  "return non-empty" when {
    "template argument is used" in {
      goToReferencesForAll(">>param2<<".r, ">>para@@m2<<")(
        """
          |TxScript GoToArgument(param1: ParamType, param2@@: ParamType) {
          |  let result = >>param2<<.someFunction()
          |  assert!(abc == >>param2<<, ErrorCode.SomeError)
          |  let param2_copy = >>param2<<
          |  >>param2<< =
          |       >>param2<< + 1
          |  >>param2<< = 0 // reset
          |  emit Mint(>>param2<<, 1)
          |  function(
          |    >>param2<<,
          |    >>param2<<
          |  )
          |  for (let mut varA = >>param2<<;
          |               varA <= 4;
          |               varA = >>param2<< + 1) {
          |     function(>>param2<<)
          |  }
          |}
          |""".stripMargin
      )
    }
  }

}
