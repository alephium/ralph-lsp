package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GoTotArgumentUsageInContractSpec extends AnyWordSpec with Matchers {

  "return empty" when {
    "argument is not used" in {
      goTo(
        """
          |Contract GoToArgument(interfa@@ce: MyInterface) {
          |  pub fn local_function(boolean: Bool) -> () {
          |    let result = blah.function()
          |  }
          |}
          |""".stripMargin
      )
    }
  }

  "return non-empty" when {
    "function argument is used" in {
      goTo(
        """
          |Contract GoToArgument() {
          |  pub fn function(param1@@: ParamType, param2: ParamType) -> () {
          |    let result = >>param1<<.someFunction()
          |    assert!(abc == >>param1<<, ErrorCode.SomeError)
          |    let param1_copy = >>param1<<
          |    param1 = >>param1<< + 1
          |    emit Mint(>>param1<<, 1)
          |    function(
          |      >>param1<<,
          |      >>param1<<
          |    )
          |    for (let mut varA = >>param1<<;
          |                 varA <= 4;
          |                 varA = >>param1<< + 1) {
          |       function(>>param1<<)
          |    }
          |  }
          |}
          |""".stripMargin
      )
    }

    "template argument is used" in {
      goTo(
        """
          |Contract GoToArgument(param1@@: ParamType, param2: ParamType) {
          |  pub fn function(param3: ParamType) -> () {
          |    let result = >>param1<<.someFunction()
          |    assert!(abc == >>param1<<, ErrorCode.SomeError)
          |    let param1_copy = >>param1<<
          |    param1 = >>param1<< + 1
          |    emit Mint(>>param1<<, 1)
          |    function(
          |      >>param1<<,
          |      >>param1<<
          |    )
          |    for (let mut varA = >>param1<<;
          |                 varA <= 4;
          |                 varA = >>param1<< + 1) {
          |       function(>>param1<<)
          |    }
          |  }
          |}
          |""".stripMargin
      )
    }
  }

}
