// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GoToLocalVariableSpec extends AnyWordSpec with Matchers {

  "return empty" when {
    "variable does not exist" when {
      "syntax is strict parsable" in {
        goToDefinition()(
          """
            |Contract GoToTest() {
            |
            |  pub fn function() -> () {
            |    // varB does not exists
            |    let varA = v@@arB
            |  }
            |
            |}
            |""".stripMargin
        )
      }

      "no Contract" in {
        goToDefinitionSoft()(
          """
            |pub fn function() -> () {
            |  // varB does not exists
            |  let varA = v@@arB
            |}
            |""".stripMargin
        )
      }

      "no function definition" in {
        goToDefinitionSoft()(
          """
            |let varA = v@@arB
            |""".stripMargin
        )
      }

      "let is misspelled" in {
        goToDefinitionSoft()(
          """
            |le varA = v@@arB
            |""".stripMargin
        )
      }
    }

    "variable exist within external scope" when {
      "for loop" in {
        goToDefinition() {
          """
            |Contract Test() {
            |
            |  fn function() -> () {
            |
            |    // index is local to the for loop
            |    for (let mut index = 0; index <= 4; index = index + 1) { bar(index) }
            |
            |    // index is not accessible from outside
            |    let copy = ind@@ex
            |
            |  }
            |
            |}
            |""".stripMargin
        }
      }
    }
  }

  "not return self" when {
    // When `let` is misspelled, this statement is an assignment, not a variable declaration.
    // So it should not jump to self.
    "let is misspelled" in {
      goToDefinitionSoft()(
        """
          |le var@@A = 123
          |""".stripMargin
      )
    }

    "let is misspelled & assignment is empty" in {
      goToDefinitionSoft()(
        """
          |le var@@A =
          |""".stripMargin
      )
    }

    "let is not defined" in {
      goToDefinitionSoft()(
        """
          |var@@A =
          |""".stripMargin
      )
    }
  }

  "return self" when {
    "variable itself is selected" when {
      "syntax is strict parsable" in {
        goToDefinition()(
          """
            |Contract Test() {
            |
            |  pub fn function() -> () {
            |    let >>var@@A<< = 123
            |  }
            |
            |}
            |""".stripMargin
        )
      }

      "Contract is not defined" in {
        goToDefinitionSoft()(
          """
            |pub fn function() -> () {
            |  let >>var@@A<< = 123
            |}
            |
            |""".stripMargin
        )
      }

      "function is not defined" in {
        goToDefinitionSoft()(
          """
            |let >>var@@A<< = 123
            |""".stripMargin
        )
      }
    }

    "duplicate variables exists" when {
      "first var is selected" when {
        "syntax is strict parsable" in {
          goToDefinition()(
            """
              |Contract Test() {
              |
              |  pub fn function() -> () {
              |    let >>var@@A<< = 123
              |    let varA = 123
              |  }
              |
              |}
              |""".stripMargin
          )
        }

        "source is not well defined" when {
          "outer Contract is not defined" in {
            goToDefinitionSoft()(
              """
                |fn function( -> () {
                |  let >>var@@A<< = 123
                |  let varA = 123
                |}
                |""".stripMargin
            )
          }

          "outer function is not defined" in {
            goToDefinitionSoft()(
              """
                |{
                |  let >>var@@A<< = 123
                |  let varA = 123
                |}
                |""".stripMargin
            )
          }

          "let is not defined" when {
            "for second varA" in {
              goToDefinitionSoft()(
                """
                  |{
                  |  let >>var@@A<< = 123
                  |  varA = 123
                  |}
                  |""".stripMargin
              )
            }

            "both varAs" in {
              goToDefinitionSoft()(
                """
                  |{
                  |  var@@A = 123
                  |  varA = 123
                  |}
                  |""".stripMargin
              )
            }

            "assignment value is not defined for the first var" in {
              goToDefinitionSoft()(
                """
                  |{
                  |  var@@A = 123
                  |  varA = 123
                  |}
                  |""".stripMargin
              )
            }

            "variable initialised value is referencing itself" in {
              goToDefinitionSoft()(
                """
                  |let >>varA<< = var@@A
                  |""".stripMargin
              )
            }
          }

        }
      }

      "second var is selected" in {
        goToDefinition()(
          """
            |Contract Test() {
            |
            |  pub fn function() -> () {
            |    let varA = 123
            |    let >>var@@A<< = 123
            |  }
            |
            |}
            |""".stripMargin
        )
      }
    }

    "a group defined" when {
      "first item is selected" in {
        goToDefinitionSoft() {
          """
            |let (>>fir@@st<<, second) = function()
            |""".stripMargin
        }
      }

      "second item is selected" in {
        goToDefinitionSoft() {
          """
            |let (first, >>secon@@d<<) =
            |""".stripMargin
        }
      }
    }
  }

  "return non-empty" when {
    "single local variable exists" when {
      "syntax is well defined" in {
        goToDefinition()(
          """
            |Contract GoToTest() {
            |
            |  pub fn function() -> () {
            |    let >>varA<< = 123
            |    let varB = var@@A
            |  }
            |
            |}
            |""".stripMargin
        )
      }

      "syntax is not well defined" when {
        "variables are defined in root scope" when {
          "accessed in root scope" in {
            goToDefinitionSoft() {
              """
                |let >>varA<< = 123
                |let varB = var@@A
                |
                |while(true) {
                |  // this is not accessed
                |  let varA = 123
                |  let varB = varA
                |}
                |
                |Contract Test() {
                |  // Some block with a copy of `varA`.
                |  // This `varA` should not get accessed.
                |  let varA = 123
                |}
                |""".stripMargin
            }
          }

          "accessed in child scopes" in {
            goToDefinitionSoft() {
              """
                |let >>varA<< = 123
                |let varB = varA
                |
                |while(true) {
                |  let >>varA<< = 123
                |  let varB = var@@A
                |}
                |
                |Contract Test() {
                |  // Some block with a copy of `varA`.
                |  // This `varA` should not get accessed.
                |  let varA = 123
                |}
                |""".stripMargin
            }
          }

          "accessed after other block-parts" when {
            "physical block is not defined" ignore {
              // FIXME: Because `Contract Test` sits between two groups of expressions,
              //        both groups of expressions do not recognise each other.
              //        This is fixed if a physical block `{}` is provided, for example in the test below.
              goToDefinitionSoft() {
                """
                  |let >>varA<< = 123
                  |let varB = varA
                  |
                  |Contract Test() { }
                  |
                  |let >>varA<< = 123
                  |let varB = va@@rA
                  |""".stripMargin
              }
            }

            "physical block is defined" in {
              goToDefinitionSoft() {
                """
                  |{
                  |  let >>varA<< = 123
                  |  let varB = varA
                  |
                  |  Contract Test() { }
                  |
                  |  let >>varA<< = 123
                  |  let varB = va@@rA
                  |}
                  |""".stripMargin
              }
            }

            "let is not assigned" in {
              goToDefinitionSoft()(
                """
                  |let >>varA<<
                  |let copy = var@@A
                  |""".stripMargin
              )
            }

            "duplicate function names & parameters" in {
              goToDefinitionSoft() {
                """
                  |{
                  |  fn function(param: Type) -> () { }
                  |
                  |  fn function(>>param<<: Type) -> () {
                  |    let copy = para@@m
                  |  }
                  |}
                  |""".stripMargin
              }
            }
          }
        }
      }
    }

    "multiple local variables exists" when {
      "defined as independent variables" when {
        "syntax is well defined" in {
          goToDefinition() {
            """
              |Contract GoToTest() {
              |
              |  pub fn function() -> () {
              |    let >>varA<< = 123
              |    let varB = var@@A
              |    let varA = ABC
              |  }
              |
              |}
              |""".stripMargin
          }
        }

        "syntax is not well defined" when {
          "variables are defined in root scope" when {
            "accessed in root scope" in {
              goToDefinitionSoft() {
                """
                  |let >>varA<< = 123
                  |let varB = var@@A
                  |let varA = ABC
                  |
                  |while(true) {
                  |  // this is not accessed
                  |  let varA = 123
                  |  let varB = varA
                  |  let varA = ABC
                  |}
                  |
                  |Contract Test() {
                  |  // Some block with a copy of `varA`.
                  |  // This `varA` should not get accessed.
                  |  let varA = 123
                  |  let varA = ABC
                  |}
                  |""".stripMargin
              }
            }

            "accessed in child scopes" in {
              goToDefinitionSoft() {
                """
                  |let >>varA<< = 123
                  |let varB = varA
                  |let >>varA<< = ABC
                  |
                  |while(true) {
                  |  let >>varA<< = 123
                  |  let varB = var@@A
                  |  let varA = ABC
                  |}
                  |
                  |Contract Test() {
                  |  // Some block with a copy of `varA`.
                  |  // This `varA` should not get accessed.
                  |  let varA = 123
                  |  let varA = ABC
                  |}
                  |""".stripMargin
              }
            }
          }
        }
      }

      "defined as a group" in {
        goToDefinitionSoft()(
          """
            |Contract GoToTest() {
            |
            |  pub fn function(>>varC<<) -> () {
            |    let (varA, varB, >>varC<<) = 123
            |    let >>varC<< = var@@C
            |    let varA = ABC
            |  }
            |
            |}
            |""".stripMargin
        )
      }
    }

    "local variable and arguments have the same name" in {
      goToDefinition()(
        """
          |Contract GoToTest(>>varA<<: Bool) {
          |
          |  pub fn function(>>varA<<: Bool) -> () {
          |    let >>varA<< = 123
          |    let varB = var@@A
          |    for (let mut varA = 0; varA <= 4; varA = varA + 1) {
          |       function(true)
          |    }
          |  }
          |}
          |""".stripMargin
      )
    }

    "variable is in an ApproveAsset expression" in {
      goToDefinition()(
        """
          |Contract GoToTest() {
          |
          |  pub fn function() -> () {
          |    let >>varA<< = 123
          |    obj.fun{builtIn!() -> ALPH: var@@A}(somethingElse)
          |  }
          |
          |}
          |""".stripMargin
      )
    }

    "variable is a tuple" when {
      "first tuple is queried" in {
        goToDefinition()(
          """
            |Contract Test() {
            |  fn test() -> () {
            |    let (>>first<<, second) = getTuple()
            |
            |    function(
            |      firs@@t,
            |      second
            |    )
            |  }
            |}
            |""".stripMargin
        )
      }

      "second tuple is queried" in {
        goToDefinition()(
          """
            |Contract Test() {
            |  fn test() -> () {
            |    let (first, >>second<<) = getTuple()
            |
            |    function(
            |      first,
            |      secon@@d
            |    )
            |  }
            |}
            |""".stripMargin
        )
      }

      "there are duplicate tuples" in {
        goToDefinition()(
          """
            |Contract Test() {
            |  fn test() -> () {
            |    let (first, >>second<<) = getTuple()
            |    let (first, >>second<<, third) = getTuple()
            |    let (first, >>second<<, third, fourth) = getTuple()
            |
            |    function(
            |      first,
            |      secon@@d
            |    )
            |  }
            |}
            |""".stripMargin
        )
      }
    }
  }

  "detect call syntax" should {
    "return variable definition" when {
      "function-call is selected" when {
        "the function does not exist" in {
          goToDefinitionSoft() {
            """
              |let >>function<< = 1
              |// no function named `function` exists, but a variable named `function` exists
              |let call = functio@@n()
              |""".stripMargin
          }
        }

        "a function with a different name exists" in {
          goToDefinitionSoft() {
            """
              |{
              |  let >>function<< = 1
              |
              |  fn test() -> () {
              |    let call = functio@@n()
              |  }
              |}
              |""".stripMargin
          }
        }

        "multiple variables exist" in {
          goToDefinitionSoft() {
            """
              |{
              |  let >>function<< =
              |
              |  fn test() -> ( {
              |    let >>function<<
              |    let call = functio@@n()
              |    let function = 3
              |  }
              |}
              |""".stripMargin
          }
        }

        "variable named `function` is declared after its usage" in {
          goToDefinitionSoft() {
            """
              |fn test() -> () {
              |  let call = functio@@n()
              |  let >>function<< = 1
              |""".stripMargin
          }
        }

        "variable named `function` is declared within inheritance" in {
          goToDefinitionSoft() {
            """
              |Abstract Contract GrandParent(>>function<<) {
              |  let >>function<< = 1
              |
              |  fn inner() -> () {
              |    // out of scope
              |    let function = 1
              |  }
              |
              |  let >>function<<
              |}
              |
              |Contract Parent() extends GrandParent() {
              |
              |  fn test() -> () {
              |    let call = functio@@n()
              |  }
              |}
              |
              |Contract Child() extends Parent() {
              |  // This is available to the `Parent`
              |  let function = 3
              |}
              |""".stripMargin
          }
        }
      }

      "variable-reference is selected" when {
        "the variable exist" in {
          goToDefinitionSoft() {
            """
              |{
              |  let >>variable<< = 1
              |  fn variable() -> {}
              |  let copy = variab@@le
              |}
              |""".stripMargin
          }
        }

        "a variable is accessed within a function" in {
          goToDefinitionSoft() {
            """
              |{
              |  let >>variable<< = 1
              |
              |  fn variable() -> () {
              |    let copy = variab@@le
              |  }
              |}
              |""".stripMargin
          }
        }

        "multiple variables exist" in {
          goToDefinitionSoft() {
            """
              |{
              |  let >>variable<< =
              |
              |  fn variable() -> ( {
              |    let >>variable<<
              |    let copy = variab@@le
              |    let function = 3
              |  }
              |}
              |""".stripMargin
          }
        }

        "variable is declared after its usage" in {
          goToDefinitionSoft() {
            """
              |fn test() -> () {
              |  let copy = variab@@le
              |  let >>variable<< = 1
              |""".stripMargin
          }
        }

        "variable is declared within inheritance" in {
          goToDefinitionSoft() {
            """
              |Abstract Contract GrandParent(>>variable<<) {
              |  let >>variable<< = 1
              |
              |  fn variable() -> () {
              |    // out of scope
              |    let variable = 1
              |  }
              |
              |  let >>variable<<
              |}
              |
              |Contract Parent() extends GrandParent() {
              |
              |  fn variable() -> () {
              |    let >>variable<< = variab@@le
              |  }
              |}
              |
              |Contract Child() extends Parent() {
              |  // This is available to the `Parent`
              |  let variable = 3
              |}
              |""".stripMargin
          }
        }
      }
    }
  }

  /**
   * Asserts inheritance search where virtual nodes are created.
   */
  "Inheritance using virtual nodes" when {
    "Parent is missing closing block and the last function contains the same identifier" in {
      goToDefinitionSoft()(
        """
          |Contract Parent {
          |
          |  let >>one<< = 1
          |
          |  def function() {
          |    // Even though the Parent is missing the closing block
          |    // this `one` is still out of scope for the `Child` contract.
          |    let one = 1
          |  }
          |""".stripMargin,
        """
          |Contract Child extends Parent {
          |   let >>one<< = on@@e.two()
          |}
          |""".stripMargin
      )
    }

    "Parent and the function both are missing closing blocks and the last function contains the same identifier" in {
      pendingUntilFixed {
        val _ =
          goToDefinitionSoft()(
            """
              |Contract Parent {
              |
              |  let >>one<< = 1
              |
              |  def function() {
              |    // This `one` escapes the scope of the function and is therefore
              |    // accessible by the `Child`.
              |    // This needs to be fixed. 
              |    let one = 1
              |""".stripMargin,
            """
              |Contract Child extends Parent {
              |   let >>one<< = on@@e.two()
              |}
              |""".stripMargin
          )
      }
    }
  }

}
