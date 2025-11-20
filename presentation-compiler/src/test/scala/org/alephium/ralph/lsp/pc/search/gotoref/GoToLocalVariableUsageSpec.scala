// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gotoref

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GoToLocalVariableUsageSpec extends AnyWordSpec with Matchers {

  "return empty" when {
    "variable is not used" when {
      "strict parseable" in {
        goToReferences() {
          """
            |Contract GoToTest() {
            |
            |  pub fn function() -> () {
            |    // varA is not used
            |    let va@@rA = varB
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
            |  let varB = 1
            |  // varA is not used
            |  let va@@rA = varB
            |}
            |""".stripMargin
        }
      }
    }
  }

  "return non-empty" when {
    "a single usage exists" when {
      "strict parseable" when {
        "comment exist" in {
          goToReferencesStrictForAll(">>var<<".r, ">>var@@<<")(
            """
              |Contract Test() {
              |  fn test() -> () {
              |    let var@@ = getVariable
              |    // Mutate
              |    >>var<< = 1
              |  }
              |}
              |""".stripMargin
          )
        }

        "no comments" in {
          goToReferencesForAll(">>varA<<".r, ">>var@@A<<")(
            """
              |Contract GoToTest() {
              |
              |  pub fn function() -> () {
              |    let va@@rA = 123
              |    let varB = >>varA<<
              |  }
              |
              |}
              |""".stripMargin
          )
        }
      }

      "soft parseable" when {
        "variable is defined before usage" in {
          goToReferencesSoftForAll(">>varA<<".r, ">>var@@A<<")(
            """
              |{
              |  let va@@rA = 123
              |  let varB = >>varA<<
              |}
              |""".stripMargin
          )
        }

        "variable is defined after usage" in {
          goToReferencesSoftForAll(">>varA<<".r, ">>var@@A<<")(
            """
              |{
              |  let varB = >>varA<<
              |  let va@@rA = 123
              |}
              |""".stripMargin
          )
        }

        "variable is defined before after usage" when {
          "before variable has no dedicated local variable defined" in {
            goToReferencesSoftForAll(">>varA<<".r, ">>var@@A<<")(
              """
                |{
                |  let varB = >>varA<< // varA is not defined before this line, it comes after, therefore, this search should result in the next line. 
                |  let va@@rA = 123    // Good! 
                |  let varB = >>varA<<
                |}
                |""".stripMargin
            )
          }

          "before variable has a dedicated local variable defined" in {
            goToReferencesSoft()(
              """
                |{
                |  let varA = 123      // dedicated `varA` variable for the next line
                |  let varB = varA     // this is defined in the line above, not after.
                |  let va@@rA = 123    // this `varA` should result only in the next line because previous line's `varA` has a variable defined 
                |  let varB = >>varA<< 
                |}
                |""".stripMargin
            )
          }
        }
      }
    }

    "multiple local variables exists" when {
      "strict parseable" in {
        goToReferencesForAll(">>varB<<".r, ">>var@@B<<")(
          """
            |Contract GoToTest() {
            |
            |  pub fn function() -> () {
            |    let varA = >>varB<<
            |    let varB@@ = varA
            |    let varC = >>varB<<
            |    obj.fun{builtIn!() -> ALPH: >>varB<<}(somethingElse)
            |  }
            |
            |}
            |""".stripMargin
        )
      }

      "soft parseable" in {
        goToReferencesSoftForAll(">>varB<<".r, ">>var@@B<<")(
          """
            |Contract Test {
            |
            |  obj.fun{builtIn!() -> ALPH: >>varB<<}(
            |  let varA = >>varB<<
            |  let varB@@ = varA
            |  let varC = >>varB<<
            |  obj.fun{builtIn!() -> ALPH: >>varB<<}(
            |
            |}
            |""".stripMargin
        )
      }
    }

    "local variable and arguments have the same name" when {
      "strict parseable" in {
        goToReferencesForAll(">>varB<<".r, ">>var@@B<<")(
          """
            |Contract GoToTest(varA: Bool) {
            |
            |  pub fn function(varA: Bool) -> () {
            |    let varA = 123
            |    let varB@@ = varA
            |    for (let mut index = >>varB<<;
            |         index <= 4;
            |         index = >>varB<< + 1) {
            |       function(>>varB<<)
            |       obj.fun{builtIn!() -> ALPH: >>varB<<}(somethingElse)
            |    }
            |  }
            |}
            |""".stripMargin
        )
      }

      "soft parseable" in {
        goToReferencesSoftForAll(">>varB<<".r, ">>var@@B<<")(
          """
            |{
            |    let = 123
            |    let varA = 123
            |    let varB@@ = varA
            |    for (let blah index = >>varB<<;
            |         index <= 4;
            |         index = >>varB<< +) {
            |       function(>>varB<<)
            |       obj.fun{builtIn!() -> ALPH: >>varB<<}(
            |                                             >>varB<<)
            |       >>varB<<
            |}
            |""".stripMargin
        )
      }
    }

    "usages exist in a for loop" when {
      "strict parseable" in {
        goToReferencesForAll(">>counter<<".r, ">>counte@@r<<")(
          """
            |Contract Test() {
            |
            |  pub fn function() -> U256 {
            |    for(let mut @@counter = 1;
            |                >>counter<< <= 4;
            |                >>counter<< =
            |                     >>counter<< + 1) {
            |      return >>counter<<
            |    }
            |    return counter
            |  }
            |}
            |
            |""".stripMargin
        )
      }

      "soft parseable" in {
        goToReferencesSoftForAll(">>counter<<".r, ">>counte@@r<<")(
          """
            |for(let mut @@counter = 1;
            |            >>counter<< <= 4;
            |            >>counter<< =
            |            >>counter<< +
            |                          >>counter<<) {
            |  >>counter<<
            |  counte
            |  return >>counter<<
            |}
            |""".stripMargin
        )
      }
    }

    "tuple usages exist" when {
      "strict parseable" in {
        goToReferencesForAll(">>second<<".r, ">>secon@@d<<")(
          """
            |Contract Test() {
            |  fn test() -> () {
            |    let (first, secon@@d, third, fourth) = getTuple()
            |
            |    function(
            |      first,
            |      >>second<<
            |    )
            |
            |    let copyFirst = first
            |    let copySecond = >>second<<
            |
            |    function2(
            |      first,
            |      >>second<<
            |    )
            |  }
            |}
            |""".stripMargin
        )
      }

      "soft parseable" in {
        goToReferencesSoftForAll(">>second<<".r, ">>secon@@d<<")(
          """
            |fn test -> {
            |  let (first, secon@@d, third, fourth) = getTuple
            |
            |  function(
            |    first,
            |    >>second<<
            |  )
            |
            |  let copyFirst = first
            |  copySecond = >>second<<
            |  >>second<< =
            |               >>second<< + 1
            |
            |  function2(
            |    first,
            |    >>second<<
            |""".stripMargin
        )
      }
    }
  }

}
