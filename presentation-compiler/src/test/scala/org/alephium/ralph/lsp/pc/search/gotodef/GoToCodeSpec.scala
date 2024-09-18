package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GoToCodeSpec extends AnyWordSpec with Matchers {

  "return empty" when {
    "typeId does not exist" in {
      goToDefinition(
        """
          |Contract GoToConstant() {
          |
          |  pub fn function(input: MyAbstrac@@t) -> () {
          |
          |  }
          |}
          |""".stripMargin
      )
    }
  }

  "return non-empty" when {
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
      goToDefinition(
        s"""
          |$types
          |
          |Contract Child() extends Paren@@t() {
          |  pub fn function() -> () { }
          |}
          |""".stripMargin
      )
    }

    "type is a function parameter" in {
      goToDefinition(
        s"""
           |$types
           |
           |Contract Child() {
           |  pub fn function(parent: Parent@@) -> () { }
           |}
           |""".stripMargin
      )
    }

    "type is a template parameter" in {
      goToDefinition(
        s"""
           |$types
           |
           |Contract Child(parent: Parent@@) {
           |  pub fn function() -> () { }
           |}
           |""".stripMargin
      )
    }

    "type is a constructor" in {
      goToDefinition(
        s"""
           |$types
           |
           |Contract Child() {
           |  pub fn function() -> () {
           |    let parent = Parent@@(blah)
           |  }
           |}
           |""".stripMargin
      )
    }

    "implemented interfaces is indirectly imported" in {
      goToStd(
        """
          |// This import does not contain the implemented INFTCollection interface,
          |// but it has INFTCollectionWithRoyalty that implements it.
          |import "std/nft_collection_with_royalty_interface"
          |
          |Abstract Contract TheContract() implements INFTCollection@@ { }
          |
          |""".stripMargin,
        Some(("Interface INFTCollection {", "INFTCollection"))
      )
    }

    "implemented interfaces is directly imported" in {
      goToStd(
        """
          |// An obvious import.
          |import "std/nft_collection_with_royalty_interface"
          |
          |Abstract Contract TheContract() implements INFTCollectionWithRoyalty@@ { }
          |
          |""".stripMargin,
        Some(("Interface INFTCollectionWithRoyalty extends INFTCollection {", "INFTCollectionWithRoyalty"))
      )
    }
  }

}
