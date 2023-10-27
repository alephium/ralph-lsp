package org.alephium.ralph.lsp.pc.sourcecode.imports

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import fastparse._
import fastparse.MultiLineWhitespace._

import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.compiler.message.SourceIndex

class ImportHandlerSpec extends AnyWordSpec with Matchers {

  "ImportHandler.parseImports" should {
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
          """)
      }

      "some imports are commented" in {
        succes("""
          //import "std/nft_interface"
          import "std/nft_interface"
          //import "std/nft_interface"
          import      "std/fungible_token_interface"
          //import      "std/fungible_token_interface"
          //import "std/nft_interface"
          """)
      }

      "they are after at then end of the line " in {
        succes("""
          import "std/nft_interface" //comment
          import      "std/fungible_token_interface" //import "std/fungible"
          """)
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
          """)
      }
    }

    "parse invalid import - failing happen at compilation" when {
      "import name is missplled" in {
        invalid("""import "std/nftint"""")
      }

      "import name is missing" in {
        invalid("""import "std/"""")
      }

      "import folder is misspelled" in {
        invalid("""import "td/nft"""")
      }

      "there is a space in import" in {
        invalid("""import "std /nft_interface"""")
      }

      "there are space before/after" in {
        invalid("""import " std/nft_interface"""")
        invalid("""import "std/nft_interface """")
        invalid("""import " std/nft_interface """")
      }

      "there are new lines before/after" in {
        invalid("""import "

          std/nft_interface"""")
        invalid("""import "std/nft_interface

          """")
        invalid("""

          import "std/nft_interface

          """")

        invalid("""|import "
                   |std/nft_interface
                   |"""".stripMargin)
      }
    }

    "find multiple errors" when {
      "all imports are wrong" in {
        invalid(
          """|import "std/nt_interface"
          |
          |import "std/"
          |""".stripMargin)
      }

      "not all imports are wrong" in {
        invalid(
          """|import "std/nt_interface"
          |import "std/nft_interface"
          |import "std/"
          |""".stripMargin)
      }
    }


    "extract import even with some contracts or any thing between imports" in {
        def check(code: String, expectedCode: String) = {
          val res = ImportHandler.parseImports(code).right.get
          res.parsedImports.map(_.name.value).toSet shouldBe Set("std/nft_interface", "std/fungible_token_interface")
          res.code shouldBe expectedCode
        }
      //We only care about imports here, contracts are parse latter by the `CompilerAccess`
      check("""|import "std/nft_interface"
               |Contract Foo(id:U256){}
               |import "std/fungible_token_interface"""".stripMargin,
           s"""|                          ${""}
               |Contract Foo(id:U256){}
               |                                     """.stripMargin)

      check("""|Contract Foo(id:U256){}
               |Contract Boo(id:U256){}
               |import "std/nft_interface"
               |import "std/fungible_token_interface"
               |Contract Boo(id:U256){}""".stripMargin,
           s"""|Contract Foo(id:U256){}
               |Contract Boo(id:U256){}
               |                          ${""}
               |                                     ${""}
               |Contract Boo(id:U256){}""".stripMargin
               )

      check("""|import "std/nft_interface"
               |Some random stuff
               |import "std/fungible_token_interface"""".stripMargin,
           s"""|                          ${""}
               |Some random stuff
               |                                     """.stripMargin,
               )

      // check("""|import "
      //          |
      //          |std/nft_interface
      //          |
      //          |"
      //          |Some random stuff
      //          |import "std/fungible_token_interface"""".stripMargin,
      //      s"""|        ${""}
      //          |
      //          |                 ${""}
      //          |
      //          | ${""}
      //          |Some random stuff
      //          |                                     """.stripMargin,
      //          )

      check("""|Some random stuff
               |import "std/nft_interface"
               |Some random stuff
               |import "std/fungible_token_interface"
               |Some random stuff
               |Contract Boo(id:U256){}""".stripMargin,
           s"""|Some random stuff
               |                          ${""}
               |Some random stuff
               |                                     ${""}
               |Some random stuff
               |Contract Boo(id:U256){}""".stripMargin,
               )
    }
  }

    implicit val compiler: CompilerAccess = CompilerAccess.ralphc

    val validCode = "Contract Boo(){ pub fn test() -> () {} }"
  //With valid and invalid import, source code should still be parsable
  def succes(code: String) = {
    val res = ImportHandler.parseImports(code ++ s"\n$validCode").right.get

    res.parsedImports.map(_.name.value).toSet shouldBe Set("std/nft_interface", "std/fungible_token_interface")
    compiler.parseContracts(res.code).isRight shouldBe true
  }

  def invalid(code: String) = {
    val res = ImportHandler.parseImports(code ++ s"\n$validCode").right.get

    res.parsedImports.size > 0 shouldBe true
    compiler.parseContracts(res.code).isRight shouldBe true
  }
}
