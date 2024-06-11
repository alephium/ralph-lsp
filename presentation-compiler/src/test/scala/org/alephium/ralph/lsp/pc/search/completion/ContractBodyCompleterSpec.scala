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
