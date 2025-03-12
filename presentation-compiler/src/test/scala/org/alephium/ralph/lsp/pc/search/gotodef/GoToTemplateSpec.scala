// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GoToTemplateSpec extends AnyWordSpec with Matchers {

  "return empty" when {
    "typeId does not exist" in {
      goToDefinition() {
        """
          |Contract GoToConstant() {
          |
          |  pub fn function(input: MyAbstrac@@t) -> () {
          |
          |  }
          |}
          |""".stripMargin
      }
    }
  }

  "return self" when {
    "type definition is selected" in {
      goToDefinition() {
        """
          |Contract >>Te@@st<<() {
          |
          |  pub fn function() -> () { }
          |
          |}
          |""".stripMargin
      }
    }

    "duplicate type definition exists" when {
      "second duplicate is selected" in {
        goToDefinition() {
          """
            |Contract Test() {
            |  pub fn function() -> () { }
            |}
            |
            |Contract >>Te@@st<<() {
            |
            |  pub fn function() -> () { }
            |
            |}
            |""".stripMargin
        }
      }
    }
  }

  "return non-empty" when {
    "strict-parseable" when {
      val types =
        """
          |TxScript >>Parent<<() {
          |  assert!()
          |}
          |
          |Abstract Contract >>Parent<<() { }
          |
          |Abstract Contract NotThis() { }
          |
          |Interface >>Parent<< {
          |  fn function() -> ()
          |}
          |
          |Interface NotThisEither {
          |  fn function() -> ()
          |}
          |
          |Contract >>Parent<<() {
          |  fn function() -> () { }
          |}
          |""".stripMargin

      "type is an inheritance" in {
        goToDefinition() {
          s"""
             |$types
             |
             |Contract Child() extends Paren@@t() {
             |  pub fn function() -> () { }
             |}
             |""".stripMargin
        }
      }

      "type is a function parameter" in {
        goToDefinition() {
          s"""
             |$types
             |
             |Contract Child() {
             |  pub fn function(parent: Paren@@t) -> () { }
             |}
             |""".stripMargin
        }
      }

      "type is a template parameter" in {
        goToDefinition() {
          s"""
             |$types
             |
             |Contract Child(parent: Paren@@t) {
             |  pub fn function() -> () { }
             |}
             |""".stripMargin
        }
      }

      "type is a constructor" in {
        goToDefinition() {
          s"""
             |$types
             |
             |Contract Child() {
             |  pub fn function() -> () {
             |    let parent = Paren@@t(blah)
             |  }
             |}
             |""".stripMargin
        }
      }
    }

    "soft-parseable" when {
      "type identifier is selected" in {
        goToDefinitionSoft() {
          """
            |Contract >>MyCode<<
            |TxScript >>MyCode<<
            |
            |MyCo@@de
            |""".stripMargin
        }
      }

      "reference call is selected" in {
        goToDefinitionSoft() {
          """
            |Contract >>MyCode<<
            |TxScript >>MyCode<<
            |
            |MyCo@@de()
            |""".stripMargin
        }
      }
    }

    "implemented interfaces is indirectly imported" in {
      goToStd(Some(("Interface INFTCollection {", "INFTCollection"))) {
        """
          |// This import does not contain the implemented INFTCollection interface,
          |// but it has INFTCollectionWithRoyalty that implements it.
          |import "std/nft_collection_with_royalty_interface"
          |
          |Abstract Contract TheContract() implements INFTCollectio@@n { }
          |
          |""".stripMargin
      }
    }

    "implemented interfaces is directly imported" in {
      goToStd(Some(("Interface INFTCollectionWithRoyalty extends INFTCollection {", "INFTCollectionWithRoyalty"))) {
        """
          |// An obvious import.
          |import "std/nft_collection_with_royalty_interface"
          |
          |Abstract Contract TheContract() implements INFTCollectionWithRoyalt@@y { }
          |
          |""".stripMargin
      }
    }
  }

  "detect call syntax" should {
    "jump to contract" when {
      "variable does not exist" in {
        goToDefinitionSoft() {
          """
            |Contract >>variable<<() { }
            |
            |Contract Test() {
            |  let vari_able = 1
            |  variab@@le
            |}
            |""".stripMargin
        }
      }
    }

    "jump to both (variable & contract)" when {
      "variable exists" in {
        goToDefinitionSoft() {
          """
            |Contract >>variable<<() { }
            |
            |Contract Test() {
            |  let >>variable<< = 1
            |  variab@@le
            |}
            |""".stripMargin
        }
      }
    }
  }

}
