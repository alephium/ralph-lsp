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
