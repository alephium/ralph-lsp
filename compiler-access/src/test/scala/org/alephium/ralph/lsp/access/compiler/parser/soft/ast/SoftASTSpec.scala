// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft.ast

import org.alephium.ralph.lsp.access.compiler.parser.soft.TestParser
import org.alephium.ralph.lsp.access.compiler.parser.soft.TestParser._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class SoftASTSpec extends AnyWordSpec with Matchers {

  "deepCopy" should {

    /**
     * The function [[SoftAST.deepCopy]] is well tested in [[TestParser.runSoftParser]]
     * by piggybacking on all existing parser test-cases.
     *
     * The following test is only to assert large trees.
     */
    "copy all instances of SourceIndex in a large SoftAST tree" in {
      // Some random code.
      val randomCode =
        parseSoft(
          """
            |// Comment one
            |// Comment two
            |
            |Contract MyContract() {
            |  fn function() ->
            |}
            |
            |let thereBeLight = 1
            |const hello = 0
            |
            |Contract AnotherContract() {
            |  fn function() -> {
            |    while(true) {
            |      // do something funky
            |    }
            |
            |     {
            |       for (a; b; c) {
            |          let copy = object.variable.function().cache.function()
            |       }
            |     }
            |  }
            |}
            |""".stripMargin
        )

      TestParser.testDeepCopy(randomCode)
    }
  }

}
