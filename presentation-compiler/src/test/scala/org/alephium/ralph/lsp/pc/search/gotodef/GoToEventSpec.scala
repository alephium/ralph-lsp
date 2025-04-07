// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GoToEventSpec extends AnyWordSpec with Matchers {

  "return empty" when {
    "event does not exists" when {
      "strict-parseable" in {
        goToDefinition() {
          """
            |Contract Test() {
            |
            |  pub fn function() -> () {
            |    emit Transfe@@r(to, amount)
            |  }
            |}
            |
            |""".stripMargin
        }
      }
    }

    "emit is a chained function call" when {
      "reference-call is search" in {
        goToDefinitionSoft()(
          """
            |Contract transfer() {
            |
            |  pub fn transfer() -> () { }
            |
            |  pub fn function() -> () {
            |    emit transfer().transfe@@r().something().value
            |  }
            |}
            |""".stripMargin
        )
      }

      "dot call value is search" in {
        goToDefinitionSoft()(
          """
            |Contract transfer() {
            |
            |  pub fn transfer() -> () { }
            |
            |  pub fn function() -> () {
            |    emit transfer().transfer().transf@@er
            |  }
            |}
            |""".stripMargin
        )
      }
    }
  }

  "return self" when {
    "strict-parseable" when {
      "an event definition is selected" in {
        goToDefinition() {
          """
            |Contract Test() extends Parent() {
            |
            |  event >>Transf@@er<<(to: Address, amount: U256)
            |
            |  pub fn function() -> () { }
            |}
            |""".stripMargin
        }
      }

      "duplicate event definitions exist" in {
        goToDefinition() {
          """
            |Contract Test() extends Parent() {
            |
            |  event Transfer(to: Address, amount: U256)
            |  event >>Transf@@er<<(to: Address, amount: U256)
            |
            |  pub fn function() -> () { }
            |}
            |""".stripMargin
        }
      }

      "duplicate event definitions exist in inheritance" in {
        goToDefinition() {
          """
            |Contract Parent() {
            |  event >>Transf@@er<<(to: Address, amount: U256)
            |  pub fn function() -> () { }
            |}
            |
            |Contract Test() extends Parent() {
            |  event Transfer(to: Address, amount: U256)
            |  pub fn function() -> () { }
            |}
            |""".stripMargin
        }
      }
    }

    "soft-parseable" when {
      "the event definition is search (self-select)" when {
        "event fields are defined" in {
          goToDefinitionSoft() {
            """
              |{
              |  event >>Transf@@er<<(to: Address, amount: U256
              |  pub fn function( -> () {
              |}
              |""".stripMargin
          }
        }

        "event fields are not defined" in {
          goToDefinitionSoft() {
            """
              |{
              |  event >>Transf@@er<<
              |  pub fn function( -> () {
              |}
              |""".stripMargin
          }
        }
      }

      "duplicate event definitions exist" when {
        "event fields are defined" in {
          goToDefinitionSoft() {
            """
              |{
              |  event Transfer(to, amount)
              |  event >>Transf@@er<<(to, amount
              |
              |  pub fn function() -> () { }
              |}
              |""".stripMargin
          }
        }

        "event fields are not defined" in {
          goToDefinitionSoft() {
            """
              |{
              |  event Transfer
              |  event >>Transf@@er<<
              |
              |  pub fn function() -> () { }
              |}
              |""".stripMargin
          }
        }
      }
    }
  }

  "search within inheritance" when {
    "an event exists" when {
      "emitting a reference call" in {
        goToDefinition()(
          """
            |Abstract Contract Parent() {
            |
            |  event TransferNotUsed(to: Address, amount: U256)
            |
            |  event >>Transfer<<(to: Address, amount: U256)
            |
            |}
            |
            |Contract Test() extends Parent() {
            |
            |  event >>Transfer<<(to: Address, amount: U256)
            |
            |  pub fn function() -> () {
            |    emit Transfe@@r(to, amount)
            |  }
            |}
            |""".stripMargin
        )
      }

      "emitting a value call" in {
        goToDefinitionSoft()(
          """
            |Abstract Contract Parent() {
            |
            |  event TransferNotUsed(to: Address, amount: U256)
            |
            |  event >>Transfer<<(to: Address, amount: U256)
            |
            |}
            |
            |Contract Test() extends Parent() {
            |
            |  event >>Transfer<<(to: Address, amount: U256)
            |
            |  pub fn function() -> () {
            |    emit Transfe@@r
            |  }
            |}
            |""".stripMargin
        )
      }

      "emitting a function call" in {
        goToDefinitionSoft()(
          """
            |Abstract Contract Parent() {
            |
            |  event TransferNotUsed(to: Address, amount: U256)
            |
            |  event >>Transfer<<(to: Address, amount: U256)
            |
            |}
            |
            |Contract Test() extends Parent() {
            |
            |  event >>Transfer<<(to: Address, amount: U256)
            |
            |  pub fn function() -> () {
            |    emit Transfe@@r.function
            |  }
            |}
            |""".stripMargin
        )
      }

      "emitting a tuple" in {
        goToDefinitionSoft()(
          """
            |Abstract Contract Parent() {
            |
            |  event TransferNotUsed(to: Address, amount: U256)
            |
            |  event Transfer(to: Address, amount: U256)
            |
            |}
            |
            |Contract >>Transfer<<() extends Parent() {
            |
            |  event Transfer(to: Address, amount: U256)
            |
            |  pub fn function() -> () {
            |    emit (Transfe@@r.function(), b, c)
            |  }
            |}
            |""".stripMargin
        )
      }
    }

    "duplicate events exist" in {
      goToDefinition()(
        """
          |Abstract Contract Parent() {
          |
          |  event >>Transfer<<(amount: U256)
          |  event >>Transfer<<(to: Address, amount: U256)
          |  event TransferNotUsed(to: Address, amount: U256)
          |  event >>Transfer<<(to: Address)
          |
          |}
          |
          |Contract Test() extends Parent() {
          |
          |  event >>Transfer<<(to: Address, amount: U256)
          |  event >>Transfer<<(amount: U256)
          |  event >>Transfer<<(to: Address)
          |
          |  pub fn function() -> () {
          |    emit Transfe@@r(to, amount)
          |  }
          |}
          |""".stripMargin
      )
    }
  }

  "Search with `emit` keyword defined" when {
    "an event, contract and a function exist with the same name" should {
      "go to the event" when {
        "the event is local" when {
          "`emit` is a reference-call" in {
            goToDefinition()(
              """
                |Contract Transfer() {
                |  fn function() -> () {}
                |}
                |
                |Contract Transfer() {
                |
                |  event >>Transfer<<(to: Address)
                |
                |  pub fn function() -> () {
                |    emit Transfe@@r(to, amount)
                |  }
                |}
                |""".stripMargin
            )
          }

          "`emit` call is not a reference-call, i.e. it's only a type value" in {
            goToDefinitionSoft()(
              """
                |Contract Transfer() {
                |  fn function() -> () {}
                |}
                |
                |Contract Transfer() {
                |
                |  event >>Transfer<<(to: Address)
                |
                |  pub fn function() -> () {
                |    emit Transfe@@r
                |  }
                |}
                |""".stripMargin
            )
          }

          "emit is value call" when {
            "event is defined locally" in {
              goToDefinitionSoft()(
                """
                  |Contract transfer() {
                  |
                  |  event >>transfer<<(to: Address)
                  |
                  |  pub fn transfer() -> () { }
                  |
                  |  pub fn function() -> () {
                  |    emit transf@@er.transfer().transfer
                  |  }
                  |}
                  |""".stripMargin
              )
            }

            "event is defined in inheritance" when {
              "event parameters are defined" in {
                goToDefinitionSoft()(
                  """
                    |Contract parent {
                    | event >>transfer<<(to: Address)
                    |}
                    |
                    |Contract transfer() extends parent {
                    |
                    |  pub fn transfer() -> () { }
                    |
                    |  pub fn function() -> () {
                    |    emit transf@@er.transfer().transfer
                    |  }
                    |}
                    |""".stripMargin
                )
              }

              "event parameters are not defined" in {
                goToDefinitionSoft()(
                  """
                    |Contract parent {
                    | event >>transfer<<
                    |}
                    |
                    |Contract transfer() extends parent {
                    |
                    |  pub fn transfer() -> () { }
                    |
                    |  pub fn function() -> () {
                    |    emit transf@@er.transfer().transfer
                    |  }
                    |}
                    |""".stripMargin
                )
              }
            }

            "event is defined globally" in {
              goToDefinitionSoft()(
                """
                  |event >>transfer<<(to: Address)
                  |
                  |Contract transfer() extends parent {
                  |
                  |  pub fn transfer() -> () { }
                  |
                  |  pub fn function() -> () {
                  |    emit transf@@er.transfer().transfer
                  |  }
                  |}
                  |""".stripMargin
              )
            }
          }

          "no event parameters exist" when {
            "event is defined locally" in {
              goToDefinitionSoft()(
                """
                  |Contract transfer() {
                  |  fn function() -> () {}
                  |}
                  |
                  |Contract transfer() {
                  |
                  |  event >>transfer<<
                  |
                  |  pub fn function() -> () {
                  |    emit transfe@@r
                  |  }
                  |}
                  |""".stripMargin
              )
            }

            "event is defined within inheritance" in {
              goToDefinitionSoft()(
                """
                  |Contract transfer {
                  |  event >>transfer<<
                  |
                  |  fn function() -> () {}
                  |}
                  |
                  |Contract transfer extends transfer {
                  |
                  |  fn function -> {
                  |    emit transfe@@r
                  |  }
                  |}
                  |""".stripMargin
              )
            }
          }
        }

        "the event is global and a duplicate function exist" in {
          goToDefinitionSoft()(
            """
              |Contract Transfer() {
              |  fn function() -> () {}
              |}
              |
              |event >>Transfer<<(to: Address)
              |
              |Contract Transfer() {
              |
              |  fn Transfer() -> () { }
              |
              |  pub fn function() -> () {
              |    emit Transfe@@r(to, amount)
              |  }
              |}
              |""".stripMargin
          )
        }
      }

      "go to the contract and the function" when {
        "the event is not defined" in {
          goToDefinitionSoft()(
            """
              |Contract >>Transfer<<() {
              |  fn function() -> () {}
              |}
              |
              |Contract >>Transfer<<() {
              |
              |  fn >>Transfer<<() -> () { }
              |
              |  pub fn function() -> () {
              |    emit Transfe@@r(to, amount)
              |  }
              |}
              |""".stripMargin
          )
        }
      }
    }
  }

  "Search with `emit` keyword not defined" when {
    "an event, contract and a function exist with the same name" should {
      "go to the event" when {
        "no other definition is found" in {
          goToDefinitionSoft()(
            """
              |Contract MyContract() {
              |
              |  event >>Transfer<<(to: Address)
              |
              |  pub fn function() -> () {
              |    Transfe@@r(to, amount)
              |  }
              |}
              |""".stripMargin
          )
        }

        "the event is global" in {
          goToDefinitionSoft()(
            """
              |event >>Transfer<<(to: Address)
              |
              |Contract MyContract() {
              |
              |  pub fn function() -> () {
              |    Transfe@@r(to, amount)
              |  }
              |}
              |""".stripMargin
          )
        }

        "the event is global and local" in {
          goToDefinitionSoft()(
            """
              |event >>Transfer<<(to: Address)
              |
              |Contract MyContract() {
              |
              |  event >>Transfer<<(to: Address)
              |
              |  pub fn function() -> () {
              |    Transfe@@r(to, amount)
              |  }
              |}
              |""".stripMargin
          )
        }
      }

      "non-event definitions" when {
        "call is a reference call" in {
          goToDefinitionSoft()(
            """
              |Contract >>Transfer<<() {
              |  fn function() -> () {}
              |}
              |
              |Contract >>Transfer<<() {
              |
              |  event Transfer(to: Address)
              |
              |  let Transfer = 1
              |
              |  fn >>Transfer<<() -> () { }
              |
              |  pub fn function() -> () {
              |    let Transfer = 1
              |    Transfe@@r(to, amount)
              |  }
              |}
              |""".stripMargin
          )
        }

        "call is a value call" in {
          goToDefinitionSoft()(
            """
              |Contract >>Transfer<<() {
              |  fn function() -> () {}
              |}
              |
              |Contract >>Transfer<<() {
              |
              |  event Transfer(to: Address)
              |
              |  let >>Transfer<< = 1
              |
              |  fn Transfer() -> () { }
              |
              |  pub fn function() -> () {
              |    let >>Transfer<< = 1
              |    Transfe@@r
              |  }
              |}
              |""".stripMargin
          )
        }
      }

    }
  }

}
