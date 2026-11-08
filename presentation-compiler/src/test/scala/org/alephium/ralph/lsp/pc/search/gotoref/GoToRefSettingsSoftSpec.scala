// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gotoref

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.wordspec.AnyWordSpec

class GoToRefSettingsSoftSpec extends AnyWordSpec {

  "event reference settings" should {
    "include differently named emitted arguments" in {
      goToReferencesSoft() {
        """Contract Test(buyer: Address) {
          |  event Transfer(t@@o: Address)
          |  fn test() -> () { emit Transfer(>>buyer<<) }
          |}""".stripMargin
      }
    }
    "keep ordinary variable references independent of event aliases" in {
      goToReferencesSoft() {
        """Contract Test(bu@@yer: Address, seller: Address) {
          | event Transfer(to: Address)
          | fn test() -> () { emit Transfer(>>buyer<<) emit Transfer(seller) }
          |}""".stripMargin
      }
    }
    "exclude emitted arguments when disabled" in {
      goToReferencesSoft(testGoToRefSetting.copy(includeEventFieldReferences = false)) {
        """Contract Test(buyer: Address) {
          |  event Transfer(t@@o: Address)
          |  fn test() -> () { emit Transfer(buyer) }
          |}""".stripMargin
      }
    }
    "add event-field definitions only with explicit reference settings" in {
      goToDefinition(testGoToDefSetting.copy(goToRefSetting = Some(testGoToRefSetting))) {
        """Contract Test(>>buyer<<: Address) {
          |  event Transfer(>>to<<: Address)
          |  fn test() -> () { emit Transfer(bu@@yer) }
          |}""".stripMargin
      }
      goToDefinition() {
        """Contract Test(>>buyer<<: Address) {
          |  event Transfer(to: Address)
          |  fn test() -> () { emit Transfer(bu@@yer) }
          |}""".stripMargin
      }
    }
    "preserve the declaration option and argument position" in {
      goToReferencesSoft(testGoToRefSetting.copy(includeDeclaration = true)) {
        """Contract Test(buyer: Address, amount: U256) {
          |  event Transfer(to: Address, >>v@@alue<<: U256)
          |  fn test() -> () { emit Transfer(buyer, >>amount<<) }
          |}""".stripMargin
      }
    }
  }

  "template override settings" should {
    "include override declarations without conflating their usages" in {
      goToReferencesSoft(
        testGoToRefSetting.copy(
          includeTemplateArgumentOverrides = true,
          goToDefSetting = testGoToDefSetting.copy(includeInheritance = false)
        )
      ) {
        """Abstract Contract Parent(val@@ue: Bool) { fn f() -> Bool { return >>value<< } }
          |Abstract Contract Child(>>value<<: Bool) extends Parent(value) { fn g() -> Bool { return value } }
          |Abstract Contract Other(value: Bool) {}""".stripMargin
      }
    }
    "preserve the enabled inheritance usage policy" in {
      goToReferencesSoft(testGoToRefSetting.copy(includeTemplateArgumentOverrides = true)) {
        """Abstract Contract Parent(val@@ue: Bool) { fn f() -> Bool { return >>value<< } }
          |Abstract Contract Child(>>value<<: Bool) extends Parent(value) {
          | fn g() -> Bool { return >>value<< }
          |}""".stripMargin
      }
    }
    "exclude override declarations when disabled" in {
      goToReferencesSoft(testGoToRefSetting.copy(includeTemplateArgumentOverrides = false)) {
        """Abstract Contract Parent(val@@ue: Bool) { fn f() -> Bool { return >>value<< } }
          |Abstract Contract Child(value: Bool) extends Parent(value) {}""".stripMargin
      }
    }
    "find the parent declaration from the child parameter" in {
      goToReferencesSoft(testGoToRefSetting.copy(includeTemplateArgumentOverrides = true)) {
        """Abstract Contract Parent(>>value<<: Bool) { fn f() -> Bool { return value } }
          |Abstract Contract Child(val@@ue: Bool) extends Parent(>>value<<) {}""".stripMargin
      }
    }
    "include the inherited parameter as an additional definition" in {
      goToDefinition(testGoToDefSetting.copy(goToRefSetting = Some(testGoToRefSetting.copy(includeTemplateArgumentOverrides = true)))) {
        """Abstract Contract Parent(>>value<<: Bool) {}
          |Abstract Contract Child(>>val@@ue<<: Bool) extends Parent(value) {}""".stripMargin
      }
    }
    "keep ordinary definition navigation on the local declaration" in {
      goToDefinition() {
        """Abstract Contract Parent(value: Bool) {}
          |Abstract Contract Child(>>val@@ue<<: Bool) extends Parent(value) {}""".stripMargin
      }
    }
  }

}
