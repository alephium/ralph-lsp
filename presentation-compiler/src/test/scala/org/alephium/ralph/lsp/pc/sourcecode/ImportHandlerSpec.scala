package org.alephium.ralph.lsp.pc.sourcecode

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import fastparse._
import fastparse.MultiLineWhitespace._

import org.alephium.ralph.lsp.compiler.message.SourceIndex

class ImportHandlerSpec extends AnyWordSpec with Matchers {

  "ImportHandler.extractStdImports" should {
    "extract valid imports" when {
      "there are new lines" in {
        succes(
          """|
          |import "std/nft_interface"
          |
          |import
          |
          |"std/fungible_token_interface"
          |
          |
          |""".stripMargin)
      }

      "they are written classicly" in {
        succes(
          """|
          |import "std/nft_interface"
          |import "std/fungible_token_interface"
          |""".stripMargin)
      }

      "there are spaces" in {
        succes("""
          import     "std/nft_interface"
          import   "std/fungible_token_interface"
          """)
      }

      ".ral extension is used" in {
        succes("""
          import "std/nft_interface.ral"
          import "std/fungible_token_interface.ral"
          """)
      }
    }

    "ignore comments" when {
      "they are classic" in {
        succes("""
          //comment
          import "std/nft_interface"
          import      "std/fungible_token_interface"
          """")
      }

      "some imports are commented" in {
        succes("""
          //import "std/nft_interface"
          import "std/nft_interface"
          //import "std/nft_interface"
          import      "std/fungible_token_interface"
          //import      "std/fungible_token_interface"
          //import "std/nft_interface"
          """")
      }

      "they are after at then end of the line " in {
        succes("""
          import "std/nft_interface" //comment
          import      "std/fungible_token_interface" //import "std/fungible"
          """")
      }

      "there are everywhere" in {
        succes("""
          // comment before
          import//import "std/fungible_token_interface"
          //
          //import "std/fungible_token_interface"
          //import "std/nft_interface"
          // comment between
          "std/nft_interface"//import "std/fungible_token_interface"
          import "std/fungible_token_interface"
          // comment after
          //
          """")
      }
    }

    "find error" when {
      "import name is missplled" in {
        fail("""import "std/nftint"""", Seq(SourceIndex(8, 10)))
      }

      "import name is missing" in {
        fail("""import "std/"""", Seq(SourceIndex(8, 4)))
      }

      "import folder is misspelled" in {
        fail("""import "td/nft"""", Seq(SourceIndex(8, 6)))
      }

      "there is a space in import" in {
        fail("""import "std /nft_interface"""", Seq(SourceIndex(8, 18)))
      }

      "there are space before/after" in {
        fail("""import " std/nft_interface"""", Seq(SourceIndex(8, 18)))
        fail("""import "std/nft_interface """", Seq(SourceIndex(8, 18)))
        fail("""import " std/nft_interface """", Seq(SourceIndex(8, 19)))
      }
    }

    "find multiple errors" when {
      "all imports are wrong" in {
        fail(
          """|import "std/nt_interface"
          |
          |import "std/"
          |""".stripMargin, Seq(SourceIndex(8, 16), SourceIndex(35,4)))
      }

      "not all imports are wrong" in {
        fail(
          """|import "std/nt_interface"
          |import "std/nft_interface"
          |import "std/"
          |""".stripMargin, Seq(SourceIndex(8, 16), SourceIndex(61,4)))
      }
    }


    "extract import even with some contracts or any thing between imports" in {
      //We only care about imports here, contracts are parse latter by the `CompilerAccess`
      succes("""
        import "std/nft_interface"
        Contract Foo(id:U256){}
        import "std/fungible_token_interface"
        """)

      succes("""
        Contract Foo(id:U256){}
        Contract Boo(id:U256){}
        import "std/nft_interface"
        import "std/fungible_token_interface"
        Contract Boo(id:U256){}
        """)

      succes("""
        import "std/nft_interface"
        Some random stuff
        import "std/fungible_token_interface"
        """)

      succes("""
        Some random stuff
        import "std/nft_interface"
        Some random stuff
        import "std/fungible_token_interface"
        Some random stuff
        Contract Boo(id:U256){}
        """)
    }
  }

  def succes(code: String) = {
    val res = ImportHandler.extractStdImports(code)
    res.map(_.imports.keys) shouldBe Right(Set("std/nft_interface", "std/fungible_token_interface"))
  }

  def fail(code: String, indexes: Seq[SourceIndex]) = {
    val res = ImportHandler.extractStdImports(code)

    res.isLeft shouldBe true

    res.left.map(_.zip(indexes).map { case (error, index) =>
      error.index shouldBe index
    })
  }
}
