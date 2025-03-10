// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.completion

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.alephium.ralph.lsp.pc.search.TestCodeProvider._

class ContractBodyCompleterSpec extends AnyWordSpec with Matchers {

  "suggest non-empty" when {
    def testItSuggestsKeywordOnly(code: String) = {
      // run completion
      val suggestions =
        suggest(code)

      val actual =
        suggestions
          .map(_.asInstanceOf[Suggestion.Keyword])
          .sortBy(_.label)

      // expect all template level keywords are suggested
      val expected =
        ContractBodyCompleter
          .suggest()
          .toList
          .sortBy(_.label)

      actual shouldBe expected
    }

    "Contract" when {
      "request is within the body" in {
        testItSuggestsKeywordOnly {
          """
            |Contract Test(bool: Bool) {
            |
            |  @@
            |
            |  fn function() -> () { }
            |}
            |""".stripMargin
        }
      }

      "request is at the template extension" in {
        testItSuggestsKeywordOnly {
          """
            |Abstract Contract Test(bool: Bool) @@ {
            |
            |  fn function() -> ()
            |}
            |""".stripMargin
        }
      }
    }

    "TxScript" when {
      "request is within the body" in {
        testItSuggestsKeywordOnly {
          """
            |TxScript MyTxScript {
            |
            |  @@
            |
            |  assert!(true, Error.SomeError)
            |
            |}
            |
            |""".stripMargin
        }
      }

      "request is at the template extension" in {
        testItSuggestsKeywordOnly {
          """
            |TxScript MyTxScript @@ {
            |
            |  assert!(true, Error.SomeError)
            |
            |}
            |""".stripMargin
        }
      }
    }

  }

}
