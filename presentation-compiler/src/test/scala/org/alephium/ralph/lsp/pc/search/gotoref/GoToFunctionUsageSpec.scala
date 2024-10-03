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
import org.alephium.ralph.lsp.pc.workspace.build.dependency.DependencyID
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

/** Test for go to function */
class GoToFunctionUsageSpec extends AnyWordSpec with Matchers {

  "annotation exploration" in {
    goToReferences(
      """
          |Contract MyContract() {
          |
          |  @using(c@@heckExternalCaller = false)
          |  pub fn function_a(boolean: Bool) -> () {
          |
          |  }
          |}
          |""".stripMargin
    )
  }

  "return empty" when {
    "function usage do not exist" in {
      goToReferences(
        """
          |Contract MyContract(interface: MyInterface) {
          |
          |  // function_a has no local usage
          |  pub fn @@function_a(boolean: Bool) -> () {
          |
          |  }
          |
          |  pub fn function_b(boolean: Bool) -> () {
          |    let call = function_c()
          |  }
          |}
          |""".stripMargin
      )
    }
  }

  "return non-empty" when {
    "function usage exist" in {
      goToReferencesForAll(">>function_a<<".r, ">>functio@@n_a<<")(
        """
          |Contract MyContract(interface: MyInterface) {
          |
          |  // function_a is selected and it has 5 call
          |  pub fn @@function_a(boolean: Bool) -> () {
          |    let call1 = >>function_a<<(true)
          |    >>function_a<<(false)
          |  }
          |
          |  pub fn function_b(boolean: Bool) -> () {
          |    let call2 = >>function_a<<(true)
          |    >>function_a<<(false)
          |    let call3 = >>function_a<<(false)
          |    >>function_a<<(true)
          |  }
          |
          |  pub fn function_c(boolean: Bool) -> () {
          |    let call4 = >>function_a<<(false)
          |    for (let mut index = 0; index <= 4; index = index + 1) {
          |      >>function_a<<(true)
          |    }
          |    let call5 = >>function_a<<(true)
          |  }
          |}
          |""".stripMargin
      )
    }

    "function usage exist within inheritance" in {
      goToReferencesForAll(">>function_a<<".r, ">>functio@@n_a<<")(
        """
          |Abstract Contract Parent() {
          |
          |  pub fn @@function_a(boolean: Bool) -> () {
          |    >>function_a<<(true)
          |    >>function_a<<(false)
          |  }
          |}
          |
          |Contract Child() extends Parent() {
          |
          |  pub fn function_b(parent: Parent,
          |                    nftCollectionId: ByteVec) -> () {
          |    >>function_a<<(true)
          |    >>function_a<<(false)
          |    parent.>>function_a<<(true)
          |  }
          |}
          |""".stripMargin
      )
    }

    "function usage exist using a contract call (no inheritance)" in {
      goToReferencesForAll(">>function_a<<".r, ">>functio@@n_a<<")(
        """
          |Contract MyContract() {
          |
          |  pub fn @@function_a(boolean: Bool) -> () {
          |    >>function_a<<(true)
          |    >>function_a<<(false)
          |  }
          |}
          |
          |Contract Main() {
          |
          |  pub fn function_b(myContract: MyContract,
          |                    nftCollectionId: ByteVec) -> () {
          |    myContract.>>function_a<<(true)
          |    MyContract(nftCollectionId).>>function_a<<(true)
          |    MyContract(nftCollectionId).>>function_a<<{callerAddress!() -> ALPH: 1 alph}(true)
          |  }
          |}
          |""".stripMargin
      )
    }

    "function is defined in a Parent and usage is via an instance of the Child" in {
      goToReferencesForAll(">>parentFunction<<".r, ">>parentFunctio@@n<<")(
        """
          |Abstract Contract Parent() {
          |
          |  pub fn @@parentFunction(boolean: Bool) -> () {
          |
          |  }
          |}
          |
          |
          |Contract Child() extends Parent() {
          |
          |  pub fn childFunction(boolean: Bool) -> () {
          |
          |  }
          |
          |}
          |
          |Contract Main() {
          |
          |  pub fn main(nftCollectionId: ByteVec,
          |              child: Child) -> () {
          |    Child(nftCollectionId).>>parentFunction<<{callerAddress!() -> ALPH: 1 alph}(true)
          |    child.>>parentFunction<<(true)
          |    Child(nftCollectionId).>>parentFunction<<(true)
          |  }
          |}
          |""".stripMargin
      )
    }

    "dependency function usage exists" when {
      "only in workspace code" in {
        goToReferences(
          dependencyId = DependencyID.BuiltIn,
          // the custom builtin library
          dependency = """
              |Interface TestBuiltIn {
              |  fn hello!() -> ()
              |
              |  fn assert!@@() -> ()
              |
              |  fn blah!() -> ()
              |}
              |""".stripMargin,
          // the developer's workspace code
          workspace = """
              |Contract Test() {
              |  pub fn function() -> () {
              |    >>assert!<<()
              |  }
              |}
              |""".stripMargin
        )

      }

      "within the dependency itself and also the workspace" in {
        goToReferences(
          dependencyId = DependencyID.BuiltIn,
          // the custom builtin library
          dependency = """
              |Interface TestBuiltIn {
              |  fn hello!() -> ()
              |
              |  fn assert!@@() -> ()
              |
              |  fn blah!() -> ()
              |}
              |
              |Contract Test() {
              |  pub fn function() -> () {
              |    >>assert!<<(true, 0)
              |  }
              |}
              |""".stripMargin,
          // the developer's workspace code (no usage)
          workspace = """
              |Contract Test() {
              |  pub fn function() -> () {
              |    >>assert!<<(false, 1)
              |  }
              |}
              |""".stripMargin
        )

      }
    }

  }

}
