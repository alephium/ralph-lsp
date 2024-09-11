// Copyright 2024 The Alephium Authors
// This file is part of the alephium project.
//
// The library is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// The library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with the library. If not, see http://www.gnu.org/licenses/.

package org.alephium.ralph.lsp.pc.search.gotoref

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GoToArgumentUsageInTxScriptSpec extends AnyWordSpec with Matchers {

  "return empty" when {
    "argument is not used" in {
      goToReferences(
        """
          |TxScript GoToArgument(interfa@@ce: MyInterface) {
          |  let result = blah.function()
          |}
          |""".stripMargin
      )
    }
  }

  "return non-empty" when {
    "template argument is used" in {
      goToReferences(
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
