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
      val root =
        parseSoft("pubfn")

      // Code "pubfn" should be recognised as a function. It is an identifier.
      root.parts should have size 1
      root.parts.head shouldBe Identifier(">>pubfn<<")
    }
  }

  "succeed" when {
    "pub an independent token" in {
      val root =
        parseSoft("pub fn")

      root.parts should have size 1
      val function = root.parts.head.asInstanceOf[SoftAST.Function]

      function.accessModifier.value shouldBe
        SoftAST.AccessModifier(
          index = indexOf(">>pub <<fn"),
          pub = Pub(">>pub<< fn"),
          postTokenSpace = Some(Space("pub>> <<fn"))
        )
    }

    "pub following an end of line" in {
      val accessModifier =
        parseAccessModifier("pub")

      accessModifier shouldBe
        SoftAST.AccessModifier(
          index = indexOf(">>pub<<"),
          pub = Pub(">>pub<<"),
          postTokenSpace = None
        )
    }

  }

}
