// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.completion

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.alephium.ralph.lsp.pc.search.TestCodeProvider._

class TypeCompleterSpec extends AnyWordSpec with Matchers {

  "return only the self type, created types, the primitive and dependency types" when {
    def doTest(code: String) = {
      val suggestion =
        suggest(code)

      val completion =
        suggestion.flatMap(_.toCompletion())

      val expected =
        expectedTypes("Object", "Abstracted")

      completion.sortBy(_.label) shouldBe expected.sortBy(_.label)
    }

    "types are requested for" when {
      "a partially typed function parameter" in {
        doTest {
          """
            |Interface Object {
            |  pub fn deposit() -> Bool
            |}
            |
            |Abstract Contract Abstracted() { }
            |
            |Contract TestContract() {
            |  fn function(int: U@@) -> () {
            |
            |  }
            |}
            |""".stripMargin
        }
      }

      "a completely typed function parameter" in {
        doTest {
          """
            |Interface Object {
            |  pub fn deposit() -> Bool
            |}
            |
            |Abstract Contract Abstracted() { }
            |
            |Contract TestContract() {
            |  fn function(int: U2@@56) -> () {
            |
            |  }
            |}
            |""".stripMargin
        }
      }

      "a partially typed template parameter" in {
        doTest {
          """
            |Interface Object {
            |  pub fn deposit() -> Bool
            |}
            |
            |Abstract Contract Abstracted() { }
            |
            |Contract TestContract(int: U@@) {
            |  fn function() -> () {
            |
            |  }
            |}
            |""".stripMargin
        }
      }

      "a completely typed template parameter" in {
        doTest {
          """
            |Interface Object {
            |  pub fn deposit() -> Bool
            |}
            |
            |Abstract Contract Abstracted() { }
            |
            |Contract TestContract(int: U@@256) {
            |  fn function() -> () {
            |
            |  }
            |}
            |""".stripMargin
        }
      }

      "an event field's type parameter" in {
        doTest {
          """
            |Interface Object {
            |  pub fn deposit() -> Bool
            |}
            |
            |Abstract Contract Abstracted() { }
            |
            |Contract TestContract(int: U256) {
            |
            |  event MyEvent(address: A@@)
            |
            |  fn function() -> () {
            |
            |  }
            |}
            |""".stripMargin
        }
      }

      "a map's key type parameter" in {
        doTest {
          """
            |Interface Object {
            |  pub fn deposit() -> Bool
            |}
            |
            |Abstract Contract Abstracted() { }
            |
            |Contract TestContract(int: U256) {
            |
            |  mapping[Add@@ress, Bool] counters
            |
            |  fn function() -> () {
            |
            |  }
            |}
            |""".stripMargin
        }
      }

      "a map's partially typed value type parameter" in {
        doTest {
          """
            |Interface Object {
            |  pub fn deposit() -> Bool
            |}
            |
            |Abstract Contract Abstracted() { }
            |
            |Contract TestContract(int: U256) {
            |
            |  mapping[Address, B@@] counters
            |
            |  fn function() -> () {
            |
            |  }
            |}
            |""".stripMargin
        }
      }

      "a map's completely typed value type parameter" in {
        doTest {
          """
            |Interface Object {
            |  pub fn deposit() -> Bool
            |}
            |
            |Abstract Contract Abstracted() { }
            |
            |Contract TestContract(int: U256) {
            |
            |  mapping[Address, B@@ool] counters
            |
            |  fn function() -> () {
            |
            |  }
            |}
            |""".stripMargin
        }
      }
    }
  }

  /**
   * Returns a list of default types that are always included.
   *
   * @param customTypes Names of additional custom types, manually created in the workspace.
   * @return A list of expected types.
   */
  private def expectedTypes(customTypes: String*): List[Completion.Class] = {
    val custom =
      customTypes.map { // convert customTypes String to Completion.Class types.
        typeName =>
          Completion.Class(
            label = typeName,
            insert = typeName,
            detail = ""
          )
      }.toList

    custom ++
      List(
        // DEPENDENCY TYPES
        Completion.Class(
          label = "TestContract",
          insert = "TestContract",
          detail = ""
        ),
        Completion.Class(
          label = "FungibleTokenUnimplemented",
          insert = "FungibleTokenUnimplemented",
          detail = ""
        ),
        Completion.Class(
          label = "INFTCollectionWithRoyalty",
          insert = "INFTCollectionWithRoyalty",
          detail = ""
        ),
        Completion.Class(
          label = "INFT",
          insert = "INFT",
          detail = ""
        ),
        Completion.Class(
          label = "INFTCollection",
          insert = "INFTCollection",
          detail = ""
        ),
        Completion.Class(
          label = "IFungibleToken",
          insert = "IFungibleToken",
          detail = ""
        ),
        // PRIMITIVE TYPES
        Completion.Class(
          label = "U256",
          insert = "U256",
          detail = ""
        ),
        Completion.Class(
          label = "I256",
          insert = "I256",
          detail = ""
        ),
        Completion.Class(
          label = "Bool",
          insert = "Bool",
          detail = ""
        ),
        Completion.Class(
          label = "ByteVec",
          insert = "ByteVec",
          detail = ""
        ),
        Completion.Class(
          label = "Address",
          insert = "Address",
          detail = ""
        )
      )
  }

}
