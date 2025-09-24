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

    "typeId is anonymous" in {
      goToDefinition() {
        """
          |Contract _() {
          |
          |  pub fn function(input: @@_) -> () {
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
        goToDefinition() {
          """
            |Contract >>MyCode<<
            |TxScript >>MyCode<<
            |
            |MyCo@@de
            |""".stripMargin
        }
      }

      "reference call is selected" in {
        goToDefinition() {
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
      goToDefStd(Some("Interface >>INFTCollection<< {")) {
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
      goToDefStd(Some("Interface >>INFTCollectionWithRoyalty<< extends INFTCollection {")) {
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
      "variable with the same name does not exist" in {
        goToDefinition() {
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

      "variable exists, but the call is a reference call" in {
        goToDefinition() {
          """
            |Contract >>variable<<() { }
            |
            |Contract Test() {
            |  event variable()
            |  let variable = 1
            |  variab@@le()
            |}
            |""".stripMargin
        }
      }
    }

    "jump to the contract and the variable" when {
      "variable exists, but the call is a method call" in {
        goToDefinition() {
          """
            |Contract >>variable<<() { }
            |
            |Contract Test(instance: variable) {
            |  let >>variable<< = instance
            |  variab@@le.function()
            |}
            |""".stripMargin
        }
      }
    }

    "jump to the variable" when {
      "variable with the same name exists" in {
        goToDefinition() {
          """
            |Contract variable() { }
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

  "go to dependency template" when {
    "interface is constructed" in {
      goToDefStd(Some("Interface >>INFTCollection<< {")) {
        """
          |import "std/nft_collection_interface"
          |
          |Contract Main() {
          |  fn test() -> () {
          |    INFTCollect@@ion(#).blah()
          |  }
          |}
          |""".stripMargin
      }
    }
  }

}
