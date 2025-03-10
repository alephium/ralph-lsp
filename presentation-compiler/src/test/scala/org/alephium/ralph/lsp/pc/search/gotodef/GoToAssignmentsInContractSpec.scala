// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GoToAssignmentsInContractSpec extends AnyWordSpec with Matchers {

  "return empty" when {
    "strict-parseable" when {
      "assigned variable does not exist" in {
        goToDefinition() {
          """
            |Contract GoToAssignment() {
            |
            |  pub fn function() -> () {
            |    counte@@r = counter + 1
            |  }
            |}
            |""".stripMargin
        }
      }

      "variable is defined in another scope" in {
        goToDefinition() {
          """
            |Contract GoToAssignment() {
            |
            |  pub fn function() -> () {
            |    while (true) {
            |      let counter = 0
            |    }
            |
            |    counte@@r = counter + 1
            |  }
            |}
            |""".stripMargin
        }
      }
    }

    "soft-parseable" when {
      "variable is defined in another scope" when {
        "function" in {
          goToDefinitionSoft() {
            """
              |fn function() -> () {
              |  {
              |    let counter = 0
              |  }
              |
              |   counte@@r = counter + 1
              |}
              |""".stripMargin
          }
        }

        "Contract" in {
          goToDefinitionSoft() {
            """
              |Contract Test() {
              |  {
              |    let counter = 0
              |  }
              |
              |   counte@@r = counter + 1
              |}
              |""".stripMargin
          }
        }
      }
    }
  }

  "return non-empty" when {
    "assigned variables exist" when {
      "locally in the function" when {
        "the code is strict-parseable" when {
          "variable is defined before its usage" in {
            goToDefinition() {
              """
                |Contract GoToAssignment() {
                |
                |  pub fn function() -> () {
                |    let >>counter<< = 0
                |    counte@@r = counter + 1
                |  }
                |}
                |""".stripMargin
            }
          }

          "variable is defined after its usage" in {
            goToDefinition() {
              """
                |Contract GoToAssignment() {
                |
                |  pub fn function() -> () {
                |    counte@@r = counter + 1
                |    let >>counter<< = 0
                |  }
                |}
                |""".stripMargin
            }
          }

          "variable is defined before and after its usage" in {
            goToDefinition() {
              """
                |Contract GoToAssignment() {
                |
                |  pub fn function() -> () {
                |    let >>counter<< = 0
                |    counte@@r = counter + 1
                |    let counter = 0
                |  }
                |}
                |""".stripMargin
            }
          }
        }

        "the code is soft-parseable" when {
          "the function and variable have unique names" when {
            "variable is defined within an inner block" in {
              goToDefinitionSoft() {
                """
                  |fn function() -> () {
                  |  {
                  |     let >>counter<< = 0
                  |     counte@@r = counter + 1
                  |  }
                  |}
                  |""".stripMargin
              }
            }

            "variable is defined without an assignment value" when {
              "the assignment's left expression is selected" in {
                goToDefinitionSoft() {
                  """
                    |fn function() -> () {
                    |  let >>counter<<
                    |  counte@@r = counter +
                    |}
                    |""".stripMargin
                }
              }

              "the assignment's right expression is selected" in {
                goToDefinitionSoft() {
                  """
                    |fn function() -> () {
                    |  let >>counter<<
                    |  counter = co@@unter + 1
                    |}
                    |""".stripMargin
                }
              }
            }
          }

          "the function and variable have duplicate names" when {
            "variable is defined within an inner block" in {
              goToDefinitionSoft() {
                """
                  |fn counter() -> () {
                  |  {
                  |     let >>counter<< = 0
                  |     counte@@r = counter + 1
                  |  }
                  |}
                  |""".stripMargin
              }
            }

            "variable is defined without an assignment value" when {
              "the assignment's left expression is selected" in {
                goToDefinitionSoft() {
                  """
                    |fn counter() -> () {
                    |  let >>counter<<
                    |  counte@@r = counter +
                    |}
                    |""".stripMargin
                }
              }

              "the assignment's right expression is selected" in {
                goToDefinitionSoft() {
                  """
                    |fn counter() -> () {
                    |  let >>counter<<
                    |  counter = co@@unter + 1
                    |}
                    |""".stripMargin
                }
              }
            }
          }
        }
      }

      "as function argument" when {
        "strict-parseable" in {
          goToDefinition() {
            """
              |Contract GoToAssignment() {
              |
              |  pub fn function(mut >>counter<<: U256) -> () {
              |    counte@@r = counter + 1
              |  }
              |}
              |""".stripMargin
          }
        }

        "soft-parseable" when {
          "argument type name is not provided" when {
            "mutable argument" in {
              goToDefinitionSoft() {
                """
                  |fn function(mut >>counter<<) -> () {
                  |  counte@@r = counter + 1
                  |}
                  |""".stripMargin
              }
            }

            "immutable argument" in {
              goToDefinitionSoft() {
                """
                  |fn function(>>counter<<) -> () {
                  |  counte@@r = counter + 1
                  |}
                  |""".stripMargin
              }
            }
          }

          "function has syntax errors" in {
            goToDefinitionSoft() {
              """
                |fn function(>>counter<< -> ( {
                |  counte@@r = counter + 1
                |""".stripMargin
            }
          }
        }
      }

      "as template argument" when {
        "strict-parseable" when {
          "single argument exists" in {
            goToDefinition() {
              """
                |Contract GoToAssignment(mut >>counter<<: U256) extends Parent(counter) {
                |
                |  pub fn function(mut bool: Bool) -> () {
                |    counte@@r = counter + 1
                |  }
                |}
                |""".stripMargin
            }
          }

          "duplicate arguments exists" when {
            "the assignment's left expression is selected" in {
              goToDefinition() {
                """
                  |Contract GoToAssignment(mut >>counter<<: U256,
                  |                            >>counter<<: U256) extends Parent(counter) {
                  |
                  |  pub fn function(mut bool: Bool) -> () {
                  |    counte@@r = counter + 1
                  |  }
                  |}
                  |""".stripMargin
              }
            }

            "the assignment's right expression is selected" in {
              goToDefinition() {
                """
                  |Contract GoToAssignment(mut >>counter<<: U256,
                  |                            >>counter<<: U256) extends Parent(counter) {
                  |
                  |  pub fn function(mut bool: Bool) -> () {
                  |    counter = counte@@r + 1
                  |  }
                  |}
                  |""".stripMargin
              }
            }
          }
        }

        "soft-parseable" when {
          "type name is not provided" when {
            "mutable argument" in {
              goToDefinitionSoft() {
                """
                  |Contract GoToAssignment(mut >>counter<<) extends Parent(counter) {
                  |
                  |  fn function(mut bool: Bool) -> () {
                  |    counte@@r = counter + 1
                  |  }
                  |}
                  |""".stripMargin
              }
            }

            "immutable argument" in {
              goToDefinitionSoft() {
                """
                  |Contract GoToAssignment(>>counter<<) extends Parent(counter) {
                  |
                  |  fn function(mut bool: Bool) -> () {
                  |    counte@@r = counter + 1
                  |  }
                  |}
                  |""".stripMargin
              }
            }

            "no function" when {
              "the assignment's left expression is selected" in {
                goToDefinitionSoft() {
                  """
                    |Contract GoToAssignment(>>counter<<) extends Parent(counter) {
                    |
                    |  counte@@r = counter + 1
                    |
                    |}
                    |""".stripMargin
                }
              }

              "the assignment's right expression is selected" in {
                goToDefinitionSoft() {
                  """
                    |Contract GoToAssignment(>>counter<<) extends Parent(counter) {
                    |
                    |  counter = counte@@r + 1
                    |
                    |}
                    |""".stripMargin
                }
              }
            }
          }
        }
      }

      "at multiple locations" when {
        "strict-parseable" in {
          goToDefinition() {
            """
              |Abstract Contract Parent2(mut >>counter<<: U256) { }
              |
              |// This is not an Abstract, but Go-To definition should still work as expected.
              |Contract Parent1(mut >>counter<<: U256,
              |                 mut >>counter<<: U256) extends Parent2(counter) {
              |
              |  // the counter parameter here is not in scope, so it should get added to search result.
              |  fn function(mut counter: U256) -> () {}
              |
              |}
              |
              |Contract GoToAssignment(mut >>counter<<: U256) extends Parent1(counter, counter) {
              |
              |  pub fn function(mut >>counter<<: U256) -> () {
              |    let mut >>counter<< = 0
              |    counte@@r = counter + 1
              |    for (let mut counter = 0; counter <= 4; counter = counter + 1) {
              |      counter = counter + 1
              |    }
              |  }
              |}
              |""".stripMargin
          }
        }

        "soft-parseable" in {
          goToDefinitionSoft() {
            """
              |Contract Parent2(>>counter<< { }
              |
              |// This is not an Abstract, but Go-To definition should still work as expected.
              |Contract Parent1(mut >>counter<<: U256,
              |                     >>counter<<) extends Parent2(counter) {
              |
              |  // the counter parameter here is not in scope, so it should get added to search result.
              |  fn function(mut counter: U256) -> () {}
              |
              |}
              |
              |Contract GoToAssignment(mut >>counter<<) extends Parent1(counter, counter) {
              |
              |  let >>counter<<
              |  let anotherCounter = 1
              |
              |  {
              |    // within a block, so it is out of scope.
              |    let mut counter = 0
              |  }
              |
              |  fn function(mut >>counter<<: U256) -> {
              |    let mut >>counter<<
              |    counter = count@@er
              |    for (let mut counter = 0; counter <= 4; counter = counter + 1) {
              |      counter = counter + 1
              |    }
              |
              |    let counter = 0
              |  }
              |
              |  let counter = 0
              |}
              |""".stripMargin
          }
        }
      }
    }
  }

}
