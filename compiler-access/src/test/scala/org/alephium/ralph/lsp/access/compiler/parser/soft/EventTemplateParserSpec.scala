package org.alephium.ralph.lsp.access.compiler.parser.soft

import org.alephium.ralph.lsp.access.compiler.parser.soft.TestParser._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.TestSoftAST._
import org.alephium.ralph.lsp.access.util.TestCodeUtil._
import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpec

class EventTemplateParserSpec extends AnyWordSpec {

  "successfully parse an event" in {
    val event = parseEventTemplate("event MyEvent(varName: TypeName")

    event.eventToken shouldBe Event(indexOf(">>event<< MyEvent(varName: TypeName"))
    event.identifier shouldBe Identifier(indexOf("event >>MyEvent<<(varName: TypeName"), "MyEvent")

    // Tuples are tested in TupleSpec, test for the index and string code here.
    event.params.index shouldBe indexOf("event MyEvent>>(varName: TypeName<<")
    event.params.toCode() shouldBe "(varName: TypeName"
    event.params.closeToken shouldBe SoftAST.TokenExpected(indexOf("event MyEvent(varName: TypeName>><<"), Token.CloseParen)
  }

}
