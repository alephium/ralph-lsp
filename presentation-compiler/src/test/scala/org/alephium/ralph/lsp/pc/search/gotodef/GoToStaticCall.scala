// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GoToStaticCall extends AnyWordSpec with Matchers {

  "return empty" when {
    "encodeFields! is invoked" when {
      "enum" in {
        goToDefBuiltIn(
          code = """
              |Contract Test() {
              |  
              |  enum Target { }
              |  
              |  fn test() -> () {
              |    Target.encodeFiel@@ds!()
              |  }
              |}
              |""".stripMargin,
          expected = None
        )
      }

      "event" in {
        goToDefBuiltIn(
          code = """
              |Contract Test() {
              |
              |  event Target(a: Type)
              |
              |  fn test() -> () {
              |    Target.encodeFiel@@ds!()
              |  }
              |}
              |""".stripMargin,
          expected = None
        )
      }

      "struct" in {
        goToDefBuiltIn(
          code = """
              |struct Target { a: Type }
              |
              |Contract Test() {
              |  fn test() -> () {
              |    Target.encodeFiel@@ds!()
              |  }
              |}
              |""".stripMargin,
          expected = None
        )
      }
    }
  }

  "access single static functions" in {
    goToDefinition()(
      """
        |Contract Test {
        |  fn >>static_function<<() -> ()
        |}
        |
        |Test.static_functi@@on
        |""".stripMargin
    )
  }

  "access duplicate static functions" in {
    goToDefinition()(
      """
        |Contract Test {
        |  fn >>static_function<<() -> ()
        |  fn >>static_function<<(paramA: A) -> ()
        |  fn >>static_function<<(paramA: A, paramB: B) -> ()
        |}
        |
        |Test.static_functi@@on
        |""".stripMargin
    )
  }

  "access within another contract" in {
    goToDefinition()(
      """
        |Contract Test {
        |  fn >>static_function<<() -> ()
        |  fn >>static_function<<(paramA: A) -> ()
        |  fn >>static_function<<(paramA: A, paramB: B) -> ()
        |}
        |
        |Contract Main {
        |  Test.static_functi@@on
        |}
        |""".stripMargin
    )
  }

  "go-to static function encodeFields!" when {
    "from global scope" in {
      goToDefBuiltIn(
        code = """
            |Contract Test { }
            |
            |Test.encodeFiel@@ds!
            |""".stripMargin,
        expected = Some("fn >>encodeFields!<<(fields:Fields) -> (ByteVec, ByteVec)")
      )
    }

    "from contract scope" in {
      goToDefBuiltIn(
        code = """
            |Contract Test { }
            |
            |Contract Main {
            |  Test.encodeFiel@@ds!
            |}
            |""".stripMargin,
        expected = Some("fn >>encodeFields!<<(fields:Fields) -> (ByteVec, ByteVec)")
      )
    }

    "from self scope" in {
      goToDefBuiltIn(
        code = """
            |Contract Main {
            |  Main.encodeFiel@@ds!
            |}
            |""".stripMargin,
        expected = Some("fn >>encodeFields!<<(fields:Fields) -> (ByteVec, ByteVec)")
      )
    }
  }

}
