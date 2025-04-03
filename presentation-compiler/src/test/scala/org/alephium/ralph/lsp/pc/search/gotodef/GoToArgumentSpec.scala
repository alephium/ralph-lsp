// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GoToArgumentSpec extends AnyWordSpec with Matchers {

  "return empty" when {
    "argument does not exists" in {
      goToDefinition() {
        """
          |Contract GoToField(interface: MyInterface) {
          |  pub fn local_function(boolean: Bool) -> () {
          |    // blah argument does not exists
          |    let result = bl@@ah.function()
          |  }
          |}
          |""".stripMargin
      }
    }

    "argument exists in another another" in {
      goToDefinition() {
        """
          |Abstract Contract Test(blah: Type) { }
          |
          |Contract GoToField(interface: MyInterface) {
          |  pub fn local_function(boolean: Bool) -> () {
          |    let result = bl@@ah.function()
          |  }
          |}
          |""".stripMargin
      }
    }

    "argument exists in another block" when {
      "the block is external to the function" in {
        goToDefinitionSoft() {
          """
            |Contract Test(interface: MyInterface) {
            |  
            |  // it's in a different block
            |  {
            |    let blah = 1
            |  }
            |  
            |  fn local_function(boolean: Bool) -> () {
            |    let result = bl@@ah.function()
            |  }
            |}
            |""".stripMargin
        }
      }

      "the block is within the function" in {
        goToDefinitionSoft() {
          """
            |Contract Test(interface: MyInterface) {
            |  
            |  fn local_function(boolean: Bool) -> () {
            |    // it's in a different block
            |    {
            |      let blah = 1
            |    }
            |  
            |    let result = bl@@ah.function()
            |  }
            |}
            |""".stripMargin
        }
      }
    }
  }

  "return self" when {
    "template argument is selected" when {
      "code is well defined" in {
        goToDefinition() {
          """
            |Contract Test(>>interfa@@ce<<: MyInterface,
            |              interface2: MyInterface) {
            |
            |  fn test(boolean: Bool) -> () { }
            |
            |}
            |""".stripMargin
        }
      }

      "Contract block does not exist" when {
        "first param is selected" in {
          goToDefinitionSoft() {
            """
              |Contract Test(>>interfa@@ce<<: MyInterface,
              |              interface2: MyInterface)
              |""".stripMargin
          }
        }

        "second param is selected" in {
          goToDefinitionSoft() {
            """
              |Contract Test(interface: MyInterface,
              |              >>interfa@@ce2<<: MyInterface)
              |""".stripMargin
          }
        }

        "param is mutable" when {
          "first param is selected" in {
            goToDefinitionSoft() {
              """
                |Contract Test(mut >>interfa@@ce<<: MyInterface,
                |                    interface2: MyInterface)
                |""".stripMargin
            }
          }

          "second param is selected" in {
            goToDefinitionSoft() {
              """
                |Contract Test(      interface: MyInterface,
                |              mut >>interfa@@ce2<<: MyInterface)
                |""".stripMargin
            }
          }
        }
      }
    }

    "function argument  is selected" when {
      "code is well defined" in {
        goToDefinition() {
          """
            |Contract Test(interface2: MyInterface) {
            |
            |  fn test(>>interfa@@ce<<: MyInterface) -> () { }
            |
            |}
            |""".stripMargin
        }
      }

      "Contract block does not exist" in {
        goToDefinitionSoft() {
          """
            |fn test(>>interfa@@ce<<: MyInterface) -> () { }
            |""".stripMargin
        }
      }

      "function has syntax errors" in {
        goToDefinitionSoft() {
          """
            |fn test(>>interfa@@ce<<: MyInterface -> ( {
            |""".stripMargin
        }
      }
    }

    "function and template argument exist with duplicate names" should {
      "select only itself" when {
        "function argument is selected" in {
          goToDefinition() {
            """
              |Contract Test(interface: MyInterface) {
              |
              |  fn test(>>interfa@@ce<<: MyInterface,
              |            interface:     MyInterface) -> () { }
              |
              |}
              |""".stripMargin
          }
        }

        "template argument is selected" in {
          goToDefinition() {
            """
              |Contract Test(>>interfa@@ce<<: MyInterface,
              |                interface:     MyInterface) {
              |
              |  fn test(interface: MyInterface) -> () { }
              |
              |}
              |""".stripMargin
          }
        }
      }
    }

    "no enclosing blocks" when {
      "type name is defined" in {
        goToDefinitionSoft() {
          """
            |>>variab@@le<<: SomeType
            |""".stripMargin
        }
      }

      "duplicate identifiers are defined" in {
        goToDefinitionSoft() {
          """
            |{
            |  >>variab@@le<<: SomeType
            |    variable:     SomeType
            |}
            |""".stripMargin
        }
      }

      "type name is not defined" in {
        goToDefinitionSoft() {
          """
            |>>variab@@le<<:
            |""".stripMargin
        }
      }

      "type name is not defined with duplicate identifiers" in {
        goToDefinitionSoft() {
          """
            |{
            |  >>variab@@le<<:
            |  variable:
            |}
            |""".stripMargin
        }
      }

      "type name is defined for another variable" in {
        goToDefinitionSoft() {
          """
            |{
            |  >>variab@@le<<:
            |  anotherVariable: SomeType
            |}
            |""".stripMargin
        }
      }

      "type name is defined for duplicate variable" in {
        goToDefinitionSoft() {
          """
            |{
            |  >>variab@@le<<:
            |  variable: SomeType
            |}
            |""".stripMargin
        }
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

    "identifier only arguments" when {
      "defined as a Contract parameter" when {
        "single param exists" in {
          goToDefinitionSoft() {
            """
              |Contract contract(>>para@@m<<)
              |""".stripMargin
          }
        }

        "duplicate params exist" when {
          "first param is selected" in {
            goToDefinitionSoft() {
              """
                |Contract contract(>>para@@m<<,
                |                  param)
                |""".stripMargin
            }
          }

          "but types are not defined" when {
            "first param is selected" in {
              goToDefinitionSoft() {
                """
                  |Contract contract(>>para@@m<<,
                  |                  param)
                  |""".stripMargin
              }
            }

            "second param is selected" in {
              goToDefinitionSoft() {
                """
                  |Contract contract(param,
                  |                  >>para@@m<<)
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
              |fn function(>>para@@m<<)
              |""".stripMargin
          }
        }

        "duplicate params exist" when {
          "first param is selected" in {
            goToDefinitionSoft() {
              """
                |fn function(>>para@@m<<,
                |            param)
                |""".stripMargin
            }
          }

          "but types are not defined" when {
            "first param is selected" in {
              goToDefinitionSoft() {
                """
                  |fn function(>>para@@m<<,
                  |            param)
                  |""".stripMargin
              }
            }

            "second param is selected" in {
              goToDefinitionSoft() {
                """
                  |fn function(param,
                  |            >>para@@m<<)
                  |""".stripMargin
              }
            }
          }
        }
      }
    }
  }

  "return non-empty" when {
    "initial character is selected" when {
      "the code is strict parseable" in {
        goToDefinition() {
          """
            |Contract GoToField(>>interface<<: MyInterface) {
            |  pub fn local_function(boolean: Bool) -> () {
            |    // first character
            |    let result = @@interface.function()
            |  }
            |}
            |""".stripMargin
        }
      }

      "the code is soft parseable" in {
        goToDefinitionSoft() {
          """
            |Contract GoToField(>>interface<<: MyInterface) {
            |  fn local_function(boolean: Bool -> {
            |    // first character
            |    let result = @@interface.function()
            |""".stripMargin
        }
      }
    }

    "mid character is selected" in {
      goToDefinition() {
        """
          |Contract GoToField(>>interface<<: MyInterface) {
          |  pub fn local_function(boolean: Bool) -> () {
          |    // mid character
          |    let result = inte@@rface.function()
          |  }
          |}
          |""".stripMargin
      }
    }

    "last character is selected" in {
      goToDefinition() {
        """
          |Contract GoToField(>>interface<<: MyInterface) {
          |  pub fn local_function(boolean: Bool) -> () {
          |    // last character
          |    let result = interfac@@e.function()
          |  }
          |}
          |""".stripMargin
      }
    }

    "function and the argument have the same name" when {
      "strict parseable" in {
        goToDefinition() {
          """
            |Abstract Contract Parent() {
            |
            |  pub fn argument_b(boolean: Bool) -> () { }
            |}
            |
            |Contract MyContract(interface: MyInterface) extends Parent() {
            |
            |  pub fn argument_b(boolean: Bool) -> () { }
            |
            |  // `argument_b` is also a function, but it should still go to the argument.
            |  pub fn argument_b(>>argument_b<<: Bool) -> () {
            |    let go_to_function = a@@rgument_b
            |    let result = blah.function()
            |  }
            |
            |  pub fn argument_b(boolean: Bool) -> () { }
            |}
            |""".stripMargin
        }
      }

      "soft parseable" in {
        goToDefinitionSoft() {
          """
            |Abstract Contract Parent() {
            |
            |  fn argument_b(boolean) -> () { }
            |}
            |
            |Contract MyContract(interface: MyInterface) extends Parent() {
            |
            |  fn argument_b(boolean) -> () { }
            |
            |  // `argument_b` is also a function, but it should still go to the argument.
            |  fn argument_b(>>argument_b<<) -> () {
            |    let go_to_function = a@@rgument_b
            |    let result = blah.function()
            |  }
            |
            |  fn argument_b(boolean: Bool) -> () { }
            |}
            |""".stripMargin
        }
      }
    }

    "there are multiple arguments with the same name" in {
      goToDefinition() {
        """
          |// the furthest argument
          |Contract GoToField(>>interface<<: MyInterface) {
          |
          |  // the nearest argument
          |  pub fn local_function(>>interface<<: MyInterface) -> () {
          |    let result = interfac@@e.function()
          |  }
          |}
          |""".stripMargin
      }
    }

    "there are duplicate arguments within inheritance" when {
      "single source-file" in {
        goToDefinition() {
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
            |    let result = para@@m.function()
            |  }
            |}
            |""".stripMargin
        }
      }

      "multiple source-files" in {
        goToDefinition()(
          """
            |Abstract Contract Parent3(>>param<<: MyParam,
            |                          >>param<<: MyParam) { }
            |""".stripMargin,
          """
            |Abstract Contract Parent2(>>param<<: MyParam,
            |                          >>param<<: MyParam) extends Parent3() { }
            |""".stripMargin,
          """
            |Abstract Contract Parent1(>>param<<: MyParam) extends Parent2() {
            |
            |  // this function also has `interface` as parameter, but it is not in scope.
            |  pub fn local_function(param: MyParam) -> () { }
            |}
            |""".stripMargin,
          """
            |Contract GoToField(>>param<<: MyParam) extends Parent1() {
            |
            |  pub fn local_function(>>param<<: MyParam) -> () {
            |    let result = para@@m.function()
            |  }
            |}
            |""".stripMargin
        )
      }
    }

    "template arguments are passed as inheritance parameter" when {
      "there are no duplicates" in {
        goToDefinition() {
          """
            |Abstract Contract SomeType() { }
            |
            |Abstract Contract Parent(param: SomeType) { }
            |
            |Abstract Contract Child(>>param<<: SomeType) extends Parent(p@@aram) { }
            |""".stripMargin
        }
      }

      "duplicates exist" when {
        "template parameter is duplicated" when {
          "first parameter is searched" in {
            goToDefinition() {
              """
                |Abstract Contract SomeType() { }
                |
                |Abstract Contract Parent(param: SomeType) { }
                |
                |Abstract Contract Child(>>param<<: SomeType,
                |                        >>param<<: SomeType) extends Parent(p@@aram, param2) { }
                |""".stripMargin
            }
          }

          "second parameter is searched" when {
            "strict-parsable" in {
              goToDefinition() {
                """
                  |Abstract Contract SomeType() { }
                  |
                  |Abstract Contract Parent(param2: SomeType) { }
                  |
                  |Abstract Contract Child(>>param2<<: SomeType,
                  |                        >>param2<<: SomeType) extends Parent(param1, p@@aram2) { }
                  |""".stripMargin
              }
            }

            "soft-parsable" in {
              goToDefinitionSoft() {
                """
                  |Abstract Contract SomeType() { }
                  |
                  |Abstract Contract Parent(param2: SomeType) { }
                  |
                  |Abstract Contract Child(>>param2<<:,
                  |                        >>param2<< extends Parent(param1, p@@aram2
                  |""".stripMargin
              }
            }
          }
        }

        "duplicated function parameters and variables" should {
          "not be included in search result" when {
            "strict parseable" in {
              goToDefinition() {
                """
                  |Abstract Contract SomeType() { }
                  |
                  |Abstract Contract Parent(param: SomeType) { }
                  |
                  |Abstract Contract Child(>>param<<: SomeType) extends Parent(p@@aram) {
                  |
                  |  // the parameter `param` is not in the scope of template `param`, it's a function level scope,
                  |  // so it should not be included in the search result.
                  |  fn function(param: SomeType) -> () { }
                  |}
                  |""".stripMargin
              }
            }

            "soft parseable" in {
              goToDefinitionSoft() {
                """
                  |let param = 1
                  |
                  |Abstract Contract SomeType() { }
                  |
                  |Abstract Contract Parent(param: SomeType) {
                  |  let param = 2
                  |}
                  |
                  |Abstract Contract Child(>>param<<) extends Parent(p@@aram) {
                  |
                  |  let param = 3
                  |
                  |  // the parameter `param` is not in the scope of template `param`, it's a function level scope,
                  |  // so it should not be included in the search result.
                  |  fn function(param: SomeType) -> () { }
                  |}
                  |
                  |let param = 4
                  |""".stripMargin
              }
            }
          }
        }
      }
    }
  }

}
