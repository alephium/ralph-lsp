package org.alephium.ralph.lsp.pc.completion

import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.client.TestClientLogger
import org.alephium.ralph.lsp.pc.log.ClientLogger
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.collection.immutable.ArraySeq

class ImportCompleterSpec extends AnyWordSpec with Matchers {

  implicit val clientLogger: ClientLogger = TestClientLogger
  implicit val file: FileAccess = FileAccess.disk
  implicit val compiler: CompilerAccess = CompilerAccess.ralphc

  "return empty" when {
    "cursor is before the quotes" in {
      TestCodeCompleter("""import @@"std" """) shouldBe empty
      TestCodeCompleter("""import @@"std/" """) shouldBe empty
    }

    "cursor is after the quotes" in {
      TestCodeCompleter("""import "std"@@ """) shouldBe empty
      TestCodeCompleter("""import "std/"@@ """) shouldBe empty
    }

    "cursor is right after the import statement" in {
      TestCodeCompleter("""import@@ "std" """) shouldBe empty
      TestCodeCompleter("""import@@ "std/" """) shouldBe empty
    }
  }

  "suggest full imports" when {
    def testContainsAllImports(actual: ArraySeq[Suggestion]) = {
      val expected =
        Array(
          "std/fungible_token_interface",
          "std/fungible_token_unimplemented",
          "std/nft_collection_interface",
          "std/nft_collection_with_royalty_interface",
          "std/nft_interface",
        ) map {
          suggestion =>
            Suggestion.Field(
              label = suggestion,
              insert = suggestion,
              detail = "",
              documentation = ""
            )
        }

      actual should contain theSameElementsAs expected
    }

    "cursor is between the quotes" in {
      testContainsAllImports(TestCodeCompleter("""import "@@" """))
    }

    "cursor is after completion's first token" in {
      testContainsAllImports(TestCodeCompleter("""import "s@@" """))
    }

    "cursor is after completion's first two tokens" in {
      testContainsAllImports(TestCodeCompleter("""import "st@@" """))
    }

    "cursor is after completion's first three tokens" in {
      testContainsAllImports(TestCodeCompleter("""import "std@@" """))
    }

    "cursor is before the forward slash" in {
      testContainsAllImports(TestCodeCompleter("""import "@@std/" """))
      testContainsAllImports(TestCodeCompleter("""import "s@@td/" """))
      testContainsAllImports(TestCodeCompleter("""import "st@@d/" """))
      testContainsAllImports(TestCodeCompleter("""import "std@@/" """))
    }
  }

  "suggest file names" when {
    "cursor is after the forward slash" in {
      val actual =
        TestCodeCompleter("""import "std/@@" """)

      val expected =
        Array(
          "fungible_token_interface",
          "fungible_token_unimplemented",
          "nft_collection_interface",
          "nft_collection_with_royalty_interface",
          "nft_interface",
        ) map {
          suggestion =>
            Suggestion.Field(
              label = suggestion,
              insert = suggestion,
              detail = "",
              documentation = ""
            )
        }

      actual should contain theSameElementsAs expected
    }
  }
}
