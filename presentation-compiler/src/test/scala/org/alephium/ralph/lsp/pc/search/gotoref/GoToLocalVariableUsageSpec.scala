// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gotoref

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GoToLocalVariableUsageSpec extends AnyWordSpec with Matchers {

  "return empty" when {
    "variable is not used" in {
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
  }

  "return non-empty" when {
    "a single usage exists" in {
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

    "multiple local variables exists" in {
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

    "local variable and arguments have the same name" in {
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

    "usages exist in a for loop" in {
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

    "tuple usages exist" in {
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
  }

}
