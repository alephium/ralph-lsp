package org.alephium.ralph.lsp.access.compiler.parser.soft

import org.alephium.ralph.lsp.access.compiler.parser.soft.TestParser._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.TestSoftAST._
import org.alephium.ralph.lsp.access.util.TestCodeUtil._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.OptionValues._

class AccessModifierParserSpec extends AnyWordSpec with Matchers {

  "fail" when {
    "pub is not an independent token" in {
      val body =
        parseSoft("pubfn")

      // Code "pubfn" should be recognised as a function. It is an identifier.
      body.parts should have size 1
      body.parts.head.part shouldBe
        Identifier(
          index = indexOf(">>pubfn<<"),
          text = "pubfn"
        )
    }
  }

  "succeed" when {
    "pub an independent token" in {
      val body =
        parseSoft("pub fn")

      body.parts should have size 1
      val function = body.parts.head.part.asInstanceOf[SoftAST.Function]

      function.accessModifier.value shouldBe
        SoftAST.AccessModifier(
          index = indexOf(">>pub <<fn"),
          pub = Pub(indexOf(">>pub<< fn")),
          postTokenSpace = Some(SpaceOne(indexOf("pub>> <<fn")))
        )
    }

    "pub following an end of line" in {
      val accessModifier =
        parseAccessModifier("pub")

      accessModifier shouldBe
        SoftAST.AccessModifier(
          index = indexOf(">>pub<<"),
          pub = Pub(indexOf(">>pub<<")),
          postTokenSpace = None
        )
    }

  }

}
