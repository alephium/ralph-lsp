// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class NativeAnnotationSpec extends AnyWordSpec with Matchers {

  "native annotations" should {
    "resolve using despite a local name collision" in {
      goToDefBuiltIn(
        code = "Contract Test() { @usi@@ng(preapprovedAssets = true) fn using() -> () {} }",
        expected = Some("fn >>using<<(")
      )
    }

    "resolve inline without parentheses" in {
      goToDefBuiltIn(
        code = "Contract Test() { @inl@@ine fn test() -> () {} }",
        expected = Some("fn >>inline<<()")
      )
    }

    "resolve std on an interface" in {
      goToDefBuiltIn(
        code = "@st@@d(id = #01) Interface Test {}",
        expected = Some("fn >>std<<(")
      )
    }

    "resolve a field while its value is missing" in {
      goToDefBuiltIn(
        code = "Contract Test() { @using(preapprovedAss@@ets = ) fn preapprovedAssets() -> () {} }",
        expected = Some(">>preapprovedAssets<<: Bool")
      )
    }

    "resolve later fields" in {
      goToDefBuiltIn(
        code = "Contract Test() { @using(updateFields = true, methodInd@@ex = 1) fn test() -> () {} }",
        expected = Some(">>methodIndex<<: U256")
      )
    }

    "resolve interface-only fields" in {
      goToDefBuiltIn(
        code = "@using(methodSele@@ctor = true) Interface Test {}",
        expected = Some(">>methodSelector<<: Bool")
      )
    }

    "not resolve unknown annotations to local functions" in {
      goToDefBuiltIn(
        code = "Contract Test() { @unkn@@own fn unknown() -> () {} }",
        expected = None
      )
    }

    "not resolve unknown fields to local functions" in {
      goToDefBuiltIn(
        code = "Contract Test() { @using(unkn@@own = ) fn unknown() -> () {} }",
        expected = None
      )
    }

    "preserve ordinary navigation in annotation values" in {
      goToDefinition() {
        "Contract Test() { const >>VALUE<< = true @using(updateFields = VAL@@UE) fn test() -> () {} }"
      }
    }

    "not expose annotation signatures as callable functions" in {
      goToDefBuiltIn(
        code = "Contract Test() { fn test() -> () { us@@ing() } }",
        expected = None
      )
    }
  }

}
