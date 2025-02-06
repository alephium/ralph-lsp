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

package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GoToArgumentSpec extends AnyWordSpec with Matchers {

  "return empty" when {
    "argument does not exists" in {
      goToDefinition()(
        """
          |Contract GoToField(interface: MyInterface) {
          |  pub fn local_function(boolean: Bool) -> () {
          |    // blah argument does not exists
          |    let result = bl@@ah.function()
          |  }
          |}
          |""".stripMargin
      )
    }
  }

  "return self" when {
    "template argument is selected" when {
      "code is well defined" in {
        goToDefinition()(
          """
            |Contract Test(>>interfa@@ce<<: MyInterface,
            |              interface2: MyInterface) {
            |
            |  fn test(boolean: Bool) -> () { }
            |
            |}
            |""".stripMargin
        )
      }

      "Contract block does not exist" when {
        "first param is selected" in {
          goToDefinitionSoft()(
            """
              |Contract Test(>>interfa@@ce<<: MyInterface,
              |              interface2: MyInterface)
              |""".stripMargin
          )
        }

        "second param is selected" in {
          goToDefinitionSoft()(
            """
              |Contract Test(interface: MyInterface,
              |              >>interfa@@ce2<<: MyInterface)
              |""".stripMargin
          )
        }
      }
    }

    "function argument  is selected" when {
      "code is well defined" in {
        goToDefinition()(
          """
            |Contract Test(interface2: MyInterface) {
            |
            |  fn test(>>interfa@@ce<<: MyInterface) -> () { }
            |
            |}
            |""".stripMargin
        )
      }

      "Contract block does not exist" in {
        goToDefinitionSoft()(
          """
            |fn test(>>interfa@@ce<<: MyInterface) -> () { }
            |""".stripMargin
        )
      }

      "function has syntax errors" in {
        goToDefinitionSoft()(
          """
            |fn test(>>interfa@@ce<<: MyInterface -> ( {
            |""".stripMargin
        )
      }
    }

    "function and template argument exist with duplicate names" should {
      "select only itself" when {
        "function argument is selected" in {
          goToDefinition()(
            """
                |Contract Test(interface: MyInterface) {
                |
                |  fn test(>>interfa@@ce<<: MyInterface) -> () { }
                |
                |}
                |""".stripMargin
          )
        }

        "template argument is selected" in {
          goToDefinition()(
            """
                |Contract Test(>>interfa@@ce<<: MyInterface) {
                |
                |  fn test(interface: MyInterface) -> () { }
                |
                |}
                |""".stripMargin
          )
        }
      }
    }

    "no enclosing blocks" when {
      "type name is defined" in {
        goToDefinitionSoft()(
          """
            |>>variab@@le<<: SomeType
            |""".stripMargin
        )
      }

      "type name is not defined" in {
        goToDefinitionSoft()(
          """
            |>>variab@@le<<:
            |""".stripMargin
        )
      }

      "type name is defined for another variable" in {
        goToDefinitionSoft()(
          """
            |{
            |  >>variab@@le<<:
            |  anotherVariable: SomeType
            |}
            |""".stripMargin
        )
      }

      "type name is defined for duplicate variable" in {
        goToDefinitionSoft()(
          """
            |{
            |  >>variab@@le<<:
            |  variable: SomeType
            |}
            |""".stripMargin
        )
      }
    }

    "mutable arguments" when {
      "defined as a Contract parameter" when {
        "single param exists" in {
          goToDefinitionSoft() {
            """
              |Contract contract(mut >>para@@m<<: Type)
              |""".stripMargin
          }
        }

        "duplicate params exist" when {
          "first param is selected" in {
            goToDefinitionSoft() {
              """
                |Contract contract(mut >>para@@m<<: Type,
                |                  mut param: Type)
                |""".stripMargin
            }
          }

          "but types are not defined" when {
            "first param is selected" in {
              goToDefinitionSoft() {
                """
                  |Contract contract(mut >>para@@m<<,
                  |                  mut param:)
                  |""".stripMargin
              }
            }

            "second param is selected" in {
              goToDefinitionSoft() {
                """
                  |Contract contract(mut param,
                  |                  mut >>para@@m<<)
                  |""".stripMargin
              }
            }
          }
        }
      }

      "defined as a function parameter" when {
        "single param exists" in {
          goToDefinitionSoft() {
            """
              |fn function(mut >>para@@m<<: Type)
              |""".stripMargin
          }
        }

        "duplicate params exist" when {
          "first param is selected" in {
            goToDefinitionSoft() {
              """
                |fn function(mut >>para@@m<<: Type,
                |            mut param: Type)
                |""".stripMargin
            }
          }

          "but types are not defined" when {
            "first param is selected" in {
              goToDefinitionSoft() {
                """
                  |fn function(mut >>para@@m<<,
                  |            mut param:)
                  |""".stripMargin
              }
            }

            "second param is selected" in {
              goToDefinitionSoft() {
                """
                  |fn function(mut param,
                  |            mut >>para@@m<<)
                  |""".stripMargin
              }
            }
          }
        }
      }
    }
  }

  "return non-empty" when {
    "initial character is selected" in {
      goToDefinitionStrict()(
        """
          |Contract GoToField(>>interface<<: MyInterface) {
          |  pub fn local_function(boolean: Bool) -> () {
          |    // first character
          |    let result = @@interface.function()
          |  }
          |}
          |""".stripMargin
      )
    }

    "mid character is selected" in {
      goToDefinitionStrict()(
        """
          |Contract GoToField(>>interface<<: MyInterface) {
          |  pub fn local_function(boolean: Bool) -> () {
          |    // mid character
          |    let result = inte@@rface.function()
          |  }
          |}
          |""".stripMargin
      )
    }

    "last character is selected" in {
      goToDefinitionStrict()(
        """
          |Contract GoToField(>>interface<<: MyInterface) {
          |  pub fn local_function(boolean: Bool) -> () {
          |    // last character
          |    let result = interface@@.function()
          |  }
          |}
          |""".stripMargin
      )
    }

    "function and the argument have the same name" in {
      goToDefinitionStrict()(
        """
          |Contract MyContract(interface: MyInterface) {
          |
          |  // argument_b is also a function, but it should still go to the argument.
          |  pub fn function_a(>>argument_b<<: Bool) -> () {
          |    let go_to_function = @@argument_b
          |    let result = blah.function()
          |  }
          |
          |  pub fn argument_b(boolean: Bool) -> () {
          |
          |  }
          |}
          |""".stripMargin
      )
    }

    "there are multiple arguments with the same name" in {
      goToDefinitionStrict()(
        """
          |// the furthest argument
          |Contract GoToField(>>interface<<: MyInterface) {
          |
          |  // the nearest argument
          |  pub fn local_function(>>interface<<: MyInterface) -> () {
          |    let result = interface@@.function()
          |  }
          |}
          |""".stripMargin
      )
    }

    "there are duplicate arguments within inheritance" in {
      goToDefinitionStrict()(
        """
          |Abstract Contract Parent3(>>param<<: MyParam,
          |                          >>param<<: MyParam) { }
          |
          |
          |Abstract Contract Parent2(>>param<<: MyParam,
          |                          >>param<<: MyParam) extends Parent3() { }
          |
          |Abstract Contract Parent1(>>param<<: MyParam) extends Parent2() {
          |
          |  // this function also has `interface` as parameter, but it is not in scope.
          |  pub fn local_function(param: MyParam) -> () { }
          |}
          |
          |Contract GoToField(>>param<<: MyParam) extends Parent1() {
          |
          |  pub fn local_function(>>param<<: MyParam) -> () {
          |    let result = param@@.function()
          |  }
          |}
          |""".stripMargin
      )
    }

    "template arguments are passed as inheritance parameter" when {
      "there are no duplicates" in {
        goToDefinitionStrict()(
          """
            |Abstract Contract SomeType() { }
            |
            |Abstract Contract Parent(param: SomeType) { }
            |
            |Abstract Contract Child(>>param<<: SomeType) extends Parent(@@param) { }
            |""".stripMargin
        )
      }

      "duplicates exist" when {
        "template parameter is duplicated" in {
          goToDefinitionStrict()(
            """
              |Abstract Contract SomeType() { }
              |
              |Abstract Contract Parent(param: SomeType) { }
              |
              |Abstract Contract Child(>>param<<: SomeType,
              |                        >>param<<: SomeType) extends Parent(@@param) { }
              |""".stripMargin
          )
        }

        "function parameter is duplicated" should {
          "not be included in search result" in {
            goToDefinitionStrict()(
              """
                |Abstract Contract SomeType() { }
                |
                |Abstract Contract Parent(param: SomeType) { }
                |
                |Abstract Contract Child(>>param<<: SomeType) extends Parent(@@param) {
                |
                |  // the parameter `param` is not in the scope of template `param`, it's a function level scope,
                |  // so it should not be included in the search result.
                |  fn function(param: SomeType) -> () { }
                |}
                |""".stripMargin
            )
          }
        }
      }

    }
  }

}
