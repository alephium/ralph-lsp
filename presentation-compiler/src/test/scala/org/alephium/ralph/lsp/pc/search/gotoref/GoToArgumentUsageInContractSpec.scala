// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gotoref

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GoToArgumentUsageInContractSpec extends AnyWordSpec with Matchers {

  "return empty" when {
    "argument is not used" in {
      goToReferences() {
        """
          |Contract GoToArgument(interfa@@ce: MyInterface) {
          |  pub fn local_function(boolean: Bool) -> () {
          |    let result = blah.function()
          |  }
          |}
          |""".stripMargin
      }
    }
  }

  "return non-empty" when {
    "function argument is used" in {
      goToReferencesForAll(">>param1<<".r, ">>para@@m1<<")(
        """
          |Contract GoToArgument() {
          |  pub fn function(param1@@: ParamType, param2: ParamType) -> () {
          |    let result = >>param1<<.someFunction()
          |    assert!(abc == >>param1<<, ErrorCode.SomeError)
          |    let param1_copy = >>param1<<
          |    >>param1<<
          |          = >>param1<< + 1
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
      goToReferencesForAll(">>param1<<".r, ">>para@@m1<<")(
        """
          |Contract GoToArgument(param1@@: ParamType, param2: ParamType) {
          |  pub fn function(param3: ParamType) -> () {
          |    let result = >>param1<<.someFunction()
          |    assert!(abc == >>param1<<, ErrorCode.SomeError)
          |    let param1_copy = >>param1<<
          |    >>param1<< =
          |        >>param1<< + 1
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

    "arguments are inherited" when {
      "from a function" in {
        goToReferencesForAll(">>param1<<".r, ">>para@@m1<<")(
          """
            |// Nothing from parent gets used
            |Abstract Contract Parent() {
            |
            |  pub fn function(param1: ParamType, param2: ParamType) -> () {
            |    let result = param1.someFunction()
            |  }
            |
            |}
            |
            |Contract Child() extends Parent() {
            |
            |  // its a function argument so parents should not output search results
            |  // search should occur locally within this function
            |  pub fn function(param1@@: ParamType, param2: ParamType) -> () {
            |    let result = >>param1<<.someFunction()
            |  }
            |
            |  pub fn function2(param1: ParamType, param2: ParamType) -> () {
            |    let result = param1.someFunction()
            |  }
            |
            |}
            |""".stripMargin
        )
      }

      "from the template" when {
        "parameter is defined in Parent" when {
          "there are no template argument duplicate names" in {
            goToReferences() {
              """
                |Abstract Contract Parent(param1@@: ParamType) {
                |
                |  pub fn function(param1: ParamType, param2: ParamType) -> () {
                |    let result = >>param1<<.someFunction()
                |  }
                |
                |}
                |
                |Contract Child() extends Parent() {
                |
                |  pub fn function(param2: ParamType) -> () {
                |    let result = >>param1<<.someFunction()
                |  }
                |
                |  pub fn function2(param1: ParamType, param2: ParamType) -> () {
                |    let result = >>param1<<.someFunction()
                |  }
                |
                |}
                |""".stripMargin
            }
          }

          "there are no duplicate names" in {
            goToReferencesForAll(">>param1<<".r, ">>para@@m1<<")(
              """
                |Abstract Contract Parent(param1@@: ParamType) {
                |
                |  pub fn function(param0: ParamType, param2: ParamType) -> () {
                |    let result = >>param1<<.someFunction()
                |  }
                |
                |}
                |
                |Contract Child() extends Parent() {
                |
                |  pub fn function(param2: ParamType) -> () {
                |    let result = >>param1<<.someFunction()
                |  }
                |
                |  pub fn function2(param0: ParamType, param2: ParamType) -> () {
                |    let result = >>param1<<.someFunction()
                |  }
                |
                |}
                |""".stripMargin
            )
          }

          "there are duplicate names" in {
            goToReferences() {
              """
                |Abstract Contract Parent(param1@@: ParamType) {
                |
                |  pub fn function(param1: ParamType, param2: ParamType) -> () {
                |    let result = >>param1<<.someFunction()
                |  }
                |
                |}
                |
                |Contract Child(param1: ParamType) extends Parent(param1) {
                |
                |  pub fn function(param2: ParamType) -> () {
                |    let result = >>param1<<.someFunction()
                |  }
                |
                |  pub fn function2(param1: ParamType, param2: ParamType) -> () {
                |    let result = >>param1<<.someFunction()
                |  }
                |
                |}
                |""".stripMargin
            }
          }
        }

        "in Child" when {
          "parameter is defined in Child" in {
            goToReferences() {
              """
                |// Parent should not have any usages
                |Abstract Contract Parent(param1: ParamType) {
                |
                |  pub fn function(param1: ParamType, param2: ParamType) -> () {
                |    let result = param1.someFunction()
                |  }
                |
                |}
                |
                |Contract Child(param1@@: ParamType)
                |  extends Parent(>>param1<<) {  // parent's input parameter should be added to usage
                |
                |  pub fn function(param2: ParamType) -> () {
                |    let result = >>param1<<.someFunction()
                |  }
                |
                |  pub fn function2(param1: ParamType, param2: ParamType) -> () {
                |    let result = >>param1<<.someFunction()
                |  }
                |
                |}
                |""".stripMargin
            }
          }
        }
      }
    }

    "template argument overrides are included" when {
      "definition is selected" in {
        goToReferences(settings = testGoToRefSetting.copy(includeTemplateArgumentOverrides = true)) {
          """
            |Abstract Contract Parent(>>para@@m1<<: ParamType) { }
            |
            |Abstract Contract Child(>>param1<<: ParamType) extends Parent(param1) { }
            |""".stripMargin
        }
      }

      "overridden definition is selected" in {
        goToReferences(settings = testGoToRefSetting.copy(includeTemplateArgumentOverrides = true)) {
          """
              |Abstract Contract Parent(>>param1<<: ParamType) { }
              |
              |Abstract Contract Child(>>param@@1<<: ParamType)
              |                  extends Parent(>>param1<<) { }
              |""".stripMargin
        }
      }
    }
  }

}
