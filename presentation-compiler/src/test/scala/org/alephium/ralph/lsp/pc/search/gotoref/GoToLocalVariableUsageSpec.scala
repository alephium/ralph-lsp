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

package org.alephium.ralph.lsp.pc.search.gotoref

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GoToLocalVariableUsageSpec extends AnyWordSpec with Matchers {

  "return empty" when {
    "variable is not used" in {
      goToReferences(
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
      )
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
          |    return >>counter<<
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
