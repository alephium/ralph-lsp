// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import org.alephium.ralph.lsp.access.compiler.parser.soft.TestParser._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.TestSoftAST._
import org.alephium.ralph.lsp.access.util.TestCodeUtil._
import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.OptionValues._

class EventParserSpec extends AnyWordSpec {

  "fail" when {
    "event is not followed by a boundary" in {
      val root =
        parseSoft("eventMyEvent")

      root.parts should have size 1
      val identifier = root.parts.head

      identifier shouldBe Identifier(">>eventMyEvent<<")
    }
  }

  "successfully parse an event" in {
    val event = parseEvent("event MyEvent(varName: TypeName")

    event.eventToken shouldBe Event(">>event<< MyEvent(varName: TypeName")
    event.identifier shouldBe Identifier("event >>MyEvent<<(varName: TypeName")

    // Tuples are tested in TupleSpec, test for the index and string code here.
    event.params.index shouldBe indexOf("event MyEvent>>(varName: TypeName<<")
    event.params.toCode() shouldBe "(varName: TypeName"
    event.params.closeToken.value shouldBe SoftAST.TokenExpected(indexOf("event MyEvent(varName: TypeName>><<"), Token.CloseParen)
  }

}
