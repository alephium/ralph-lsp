// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gotoref

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

/**
 * Inverse test for [[org.alephium.ralph.lsp.pc.search.gotodef.ScopeWalkerSpec]]
 * that implements the same tests, but for references.
 */
class ScopeWalkerUsageSpec extends AnyWordSpec with Matchers {

  "allow variable access" when {
    "defined outside the scope of a block" when {
      "defined before usage" when {
        "strict parseable" in {
          goToReferencesForAll(">>variable<<".r, ">>variabl@@e<<")(
            """
              |Contract Test() {
              |
              |  pub fn test() -> () {
              |    let variab@@le = 1
              |    while (true) {
              |      let copyVar = >>variable<<
              |    }
              |    let variable = 1
              |  }
              |
              |}
              |""".stripMargin
          )
        }

        "soft parseable" when {
          "within function block" in {
            goToReferencesSoftForAll(">>variable<<".r, ">>variabl@@e<<")(
              """
                |Contract Test {
                |
                |  fn test -> {
                |    let variab@@le = 1
                |    while (true) {
                |      let copyVar = >>variable<<
                |    }
                |    let variable = 1
                |  }
                |
                |}
                |""".stripMargin
            )
          }

          "within contract block" in {
            goToReferencesSoftForAll(">>variable<<".r, ">>variabl@@e<<")(
              """
                |Contract Test {
                |
                |  let variab@@le = 1
                |  while (true) {
                |    let copyVar = >>variable<<
                |  }
                |  let variable = 1
                |
                |}
                |""".stripMargin
            )
          }

          "a block" in {
            goToReferencesSoftForAll(">>variable<<".r, ">>variabl@@e<<")(
              """
                |{
                |  let variab@@le = 1
                |  while (true) {
                |    let copyVar = >>variable<<
                |  }
                |  let variable = 1
                |}
                |""".stripMargin
            )
          }
        }
      }

      "defined after usage" should {
        "go-to the first definition" when {
          "strict parseable" in {
            goToReferencesForAll(">>variable<<".r, ">>variabl@@e<<")(
              """
                |Contract Test() {
                |
                |  pub fn test() -> () {
                |    while (true) {
                |      let copyVar = >>variable<<
                |    }
                |    let variab@@le = 1
                |    let variable = 1
                |  }
                |
                |}
                |""".stripMargin
            )
          }

          "soft parseable" when {
            "within function block" in {
              goToReferencesSoftForAll(">>variable<<".r, ">>variabl@@e<<")(
                """
                  |Contract Test {
                  |
                  |  pub fn test {
                  |    while (true) {
                  |      let copyVar = >>variable<<
                  |    }
                  |    let variab@@le = 1
                  |    let variable = 1
                  |  }
                  |
                  |}
                  |""".stripMargin
              )
            }

            "within contract block" in {
              goToReferencesSoftForAll(">>variable<<".r, ">>variabl@@e<<")(
                """
                  |Contract Test {
                  |
                  |  while (true) {
                  |    let copyVar = >>variable<<
                  |  }
                  |  let variab@@le = 1
                  |  let variable = 1
                  |
                  |}
                  |""".stripMargin
              )
            }

            "a block" in {
              goToReferencesSoftForAll(">>variable<<".r, ">>variabl@@e<<")(
                """
                  |{
                  |  while (true) {
                  |    let copyVar = >>variable<<
                  |  }
                  |  let variab@@le = 1
                  |  let variable = 1
                  |}
                  |""".stripMargin
              )
            }
          }
        }

        "prioritise local definition over outside definition" when {
          "strict parseable" in {
            goToReferencesForAll(">>variable<<".r, ">>variabl@@e<<")(
              """
                |Contract Test() {
                |
                |  pub fn test() -> () {
                |    while (true) {
                |      let variabl@@e = 1
                |      let copyVar = >>variable<<
                |    }
                |    let variable = 1
                |    let variable = 1
                |  }
                |
                |}
                |""".stripMargin
            )
          }

          "soft parseable" when {
            "within function block" in {
              goToReferencesSoftForAll(">>variable<<".r, ">>variabl@@e<<")(
                """
                  |Contract Test {
                  |
                  |  fn test -> {
                  |    while (true) {
                  |      let variabl@@e = 1
                  |      let copyVar = >>variable<<
                  |    }
                  |    let variable = 1
                  |    let variable = 1
                  |  }
                  |
                  |}
                  |""".stripMargin
              )
            }

            "within contract block" in {
              goToReferencesSoftForAll(">>variable<<".r, ">>variabl@@e<<")(
                """
                  |Contract Test {
                  |
                  |  while (true) {
                  |    let variabl@@e = 1
                  |    let copyVar = >>variable<<
                  |  }
                  |  let variable = 1
                  |  let variable = 1
                  |
                  |}
                  |""".stripMargin
              )
            }

            "within a block" in {
              goToReferencesSoftForAll(">>variable<<".r, ">>variabl@@e<<")(
                """
                  |{
                  |  while (true) {
                  |    let variabl@@e = 1
                  |    let copyVar = >>variable<<
                  |  }
                  |  let variable = 1
                  |  let variable = 1
                  |}
                  |""".stripMargin
              )
            }
          }
        }
      }

      "defined in a for loop" when {
        "strict parseable" in {
          goToReferencesForAll(">>index<<".r, ">>inde@@x<<")(
            """
              |Contract Test() {
              |
              |  pub fn test() -> () {
              |    while (true) {
              |      for (let mut inde@@x = 0;
              |                   >>index<< < 2;
              |                   >>index<< =
              |                             >>index<< + 1) {
              |        let copyIndex = >>index<<
              |      }
              |      // these are not in the search result
              |      let copyIndex = index
              |    }
              |  }
              |
              |}
              |""".stripMargin
          )
        }

        "soft parseable" when {
          "within function block" in {
            goToReferencesSoftForAll(">>index<<".r, ">>inde@@x<<")(
              """
                |Contract Test {
                |
                |  fn test {
                |    while (true) {
                |      for (let mut inde@@x = 0;
                |                   >>index<< < 2;
                |                   >>index<< =
                |                             >>index<< + 1) {
                |        let copyIndex = >>index<<
                |      }
                |      // these are not in the search result
                |      index
                |      let copyIndex = index
                |    }
                |  }
                |
                |}
                |""".stripMargin
            )
          }

          "within contract block" in {
            goToReferencesSoftForAll(">>index<<".r, ">>inde@@x<<")(
              """
                |Contract Test {
                |
                |  while (true) {
                |    for (let mut inde@@x = 0;
                |                 >>index<< < 2;
                |                 >>index<< =
                |                           >>index<< + 1) {
                |      >>index<<
                |      let copyIndex = >>index<<
                |    }
                |    // these are not in the search result
                |    index
                |    let copyIndex = index
                |  }
                |
                |}
                |""".stripMargin
            )
          }

          "within a block" in {
            goToReferencesSoftForAll(">>index<<".r, ">>inde@@x<<")(
              """
                |{
                |  while (true) {
                |    for (let mut inde@@x = 0;
                |                 >>index<< < 2;
                |                 >>index<< =
                |                           >>index<< + 1) {
                |      >>index<<
                |      let copyIndex = >>index<<
                |    }
                |    // these are not in the search result
                |    index
                |    let copyIndex = index
                |  }
                |}
                |""".stripMargin
            )
          }
        }
      }
    }

    "defined within one of the nested blocks and accessed in another" when {
      "define before usage" when {
        "strict parseable" in {
          goToReferencesForAll(">>variable<<".r, ">>variabl@@e<<")(
            """
              |Contract Test() {
              |
              |  pub fn test() -> () {
              |    while (true) {
              |      let variab@@le = 1
              |      while (true) {
              |        let b = inner2
              |        for (let mut index = 0; index < 2; index = >>variable<< + 1) {
              |          while (true) {
              |            if (true) {
              |              let copyVar = >>variable<<
              |            }
              |          }
              |        }
              |      }
              |    }
              |  }
              |
              |}
              |""".stripMargin
          )
        }

        "soft parseable" in {
          goToReferencesSoftForAll(">>variable<<".r, ">>variabl@@e<<")(
            """
              |while (true) {
              |  let variab@@le = 1
              |  while (true) {
              |    let b = inner2
              |    for (let mut index = 0; index < 2; index = >>variable<< + 1) {
              |      while (true) {
              |        if (true) {
              |          let copyVar = >>variable<<
              |        }
              |      }
              |    }
              |  }
              |}
              |""".stripMargin
          )
        }
      }

      "define after usage" when {
        "strict parseable" in {
          goToReferencesForAll(">>variable<<".r, ">>variabl@@e<<")(
            """
              |Contract Test() {
              |
              |  pub fn test() -> () {
              |    while (true) {
              |      while (true) {
              |        let b = inner2
              |        while (true) {
              |          while (true) {
              |            let copyVar = >>variable<<
              |          }
              |        }
              |      }
              |      let variabl@@e = 1
              |      let variable = 1
              |    }
              |  }
              |
              |}
              |""".stripMargin
          )
        }

        "soft parseable" in {
          goToReferencesSoftForAll(">>variable<<".r, ">>variabl@@e<<")(
            """
              |while (true) {
              |  while (true) {
              |    let b = inner2
              |    while (true) {
              |      while (true) {
              |        let copyVar = >>variable<<
              |      }
              |    }
              |  }
              |  let variabl@@e = 1
              |  let variable = 1
              |}
              |""".stripMargin
          )
        }
      }
    }
  }

  "disallow variable access" when {
    "definition is in a different scope" when {
      "defined before usage" when {
        "strict parseable" in {
          goToReferences() {
            """
              |Contract Test() {
              |
              |  pub fn test() -> () {
              |    while(true) {
              |      let variab@@le = 1
              |    }
              |    while (true) {
              |      let copyVar = variable
              |    }
              |  }
              |
              |}
              |""".stripMargin
          }
        }

        "soft parseable" in {
          goToReferencesSoft() {
            """
              |{
              |  // defined in a local scope
              |  {
              |    let variab@@le = 1
              |  }
              |  
              |  // does not have access to local scope
              |  let copyVar = variable
              |}
              |""".stripMargin
          }
        }
      }

      "defined after usage" when {
        "strict parseable" in {
          goToReferences() {
            """
              |Contract Test() {
              |
              |  pub fn test() -> () {
              |    while (true) {
              |      let copyVar = variable
              |    }
              |    for (let mut index = 0; index < 2; index = index + 1) {
              |      let varia@@ble = 1
              |    }
              |  }
              |
              |}
              |""".stripMargin
          }
        }

        "soft parseable" in {
          goToReferencesSoft() {
            """
              |{
              |  // Scope 1
              |  {
              |    let copyVar = variable
              |  }
              |
              |  // Scope 2
              |  {
              |    let varia@@ble = 1
              |  }
              |}
              |""".stripMargin
          }
        }
      }

      "defined after usage but in an inner scope" when {
        "strict parseable" in {
          goToReferences() {
            """
              |Contract Test() {
              |
              |  pub fn test() -> () {
              |    while (true) {
              |      let copyVar = variable
              |      for (let mut index = 0; index < 2; index = index + 1) {
              |        let varia@@ble = 1
              |      }
              |    }
              |  }
              |
              |}
              |""".stripMargin
          }
        }

        "soft parseable" in {
          goToReferencesSoft() {
            """
              |{
              |  let copyVar = variable
              |  for {
              |    let varia@@ble = 1
              |  }
              |}
              |""".stripMargin
          }
        }
      }

      "defined in a for loop" when {
        "strict parseable" in {
          goToReferencesForAll(">>index<<".r, ">>inde@@x<<")(
            """
              |Contract Test() {
              |
              |  pub fn test() -> () {
              |    while (true) {
              |      let copyIndex = index
              |      for (let mut ind@@ex = 0;
              |                   >>index<< < 2;
              |                   >>index<< =
              |                              >>index<< + 1) {
              |        let variable = 1
              |      }
              |    }
              |  }
              |
              |}
              |""".stripMargin
          )
        }

        "soft parseable" in {
          goToReferencesSoftForAll(">>index<<".r, ">>inde@@x<<")(
            """
              |{
              |  let copyIndex = index
              |  for (let mut ind@@ex = 0;
              |               >>index<< < 2;
              |               >>index<< =
              |                           >>index<< + 1) {
              |    let variable = 1
              |  }
              |}
              |""".stripMargin
          )
        }
      }
    }
  }

}
