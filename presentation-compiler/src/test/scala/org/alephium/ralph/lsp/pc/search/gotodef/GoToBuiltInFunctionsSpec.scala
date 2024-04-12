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

package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GoToBuiltInFunctionsSpec extends AnyWordSpec with Matchers {

  "return empty" when {
    "built-in does not exists" in {
      goToBuiltIn(
        code = """
            |Contract Test() {
            |  pub fn function() -> () {
            |    bla@@h!()
            |  }
            |}
            |""".stripMargin,
        expected = None
      )
    }
  }

  "return non-empty" when {
    "assert!" in {
      goToBuiltIn(
        code = """
            |Contract Test() {
            |  pub fn function() -> () {
            |    @@assert!()
            |  }
            |}
            |""".stripMargin,
        expected = Some("""fn assert!(condition:Bool, errorCode:U256) -> ()""")
      )
    }

    "verifyAbsoluteLocktime!" in {
      goToBuiltIn(
        code = """
            |Contract Test() {
            |  pub fn function() -> () {
            |    for (let mut index = 0; index <= 4; index = index + 1) {
            |      @@verifyAbsoluteLocktime!()
            |    }
            |  }
            |}
            |""".stripMargin,
        expected = Some("""fn verifyAbsoluteLocktime!(lockUntil:U256) -> ()""")
      )
    }

    "there are duplicate local and built-in function names" in {
      goToBuiltIn(
        code = """
            |Contract Test() {
            |
            |  // this function should not get selected.
            |  fn assert() -> () {
            |
            |  }
            |
            |  pub fn function() -> () {
            |    @@assert!()
            |  }
            |}
            |""".stripMargin,
        expected = Some("""fn assert!(condition:Bool, errorCode:U256) -> ()""")
      )
    }
  }

}
