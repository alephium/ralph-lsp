package org.alephium.ralph.lsp.pc.completion

import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.client.TestClientLogger
import org.alephium.ralph.lsp.pc.log.ClientLogger
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

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

    "cursor is between the quotes" in {
      TestCodeCompleter("""import "@@" """) should contain theSameElementsAs expected
      TestCodeCompleter("""import "s@@" """) should contain theSameElementsAs expected
      TestCodeCompleter("""import "st@@" """) should contain theSameElementsAs expected
      TestCodeCompleter("""import "std@@" """) should contain theSameElementsAs expected
    }

    "cursor is before the forward slash" in {
      TestCodeCompleter("""import "@@std/" """) should contain theSameElementsAs expected
      TestCodeCompleter("""import "s@@td/" """) should contain theSameElementsAs expected
      TestCodeCompleter("""import "st@@d/" """) should contain theSameElementsAs expected
      TestCodeCompleter("""import "std@@/" """) should contain theSameElementsAs expected
    }
  }

  "suggest file names" when {
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

    "cursor is after the forward slash" in {
      TestCodeCompleter("""import "std/@@" """) should contain theSameElementsAs expected
      TestCodeCompleter("""import "std/fungible@@" """) should contain theSameElementsAs expected
      TestCodeCompleter("""import "std/fungible_@@" """) should contain theSameElementsAs expected
      TestCodeCompleter("""import "std/nft@@" """) should contain theSameElementsAs expected
      TestCodeCompleter("""import "std/nft_@@" """) should contain theSameElementsAs expected
    }
  }
}
