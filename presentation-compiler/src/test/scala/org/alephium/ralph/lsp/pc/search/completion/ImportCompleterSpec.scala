package org.alephium.ralph.lsp.pc.search.completion

import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.client.TestClientLogger
import org.alephium.ralph.lsp.pc.log.ClientLogger
import org.alephium.ralph.lsp.pc.search.TestCodeSearcher
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ImportCompleterSpec extends AnyWordSpec with Matchers {

  implicit val clientLogger: ClientLogger = TestClientLogger
  implicit val file: FileAccess = FileAccess.disk
  implicit val compiler: CompilerAccess = CompilerAccess.ralphc

  "return empty" when {
    "cursor is before the quotes" in {
      TestCodeSearcher[Suggestion]("""import @@"std" """) shouldBe empty
      TestCodeSearcher[Suggestion]("""import @@"std/" """) shouldBe empty
    }

    "cursor is after the quotes" in {
      TestCodeSearcher[Suggestion]("""import "std"@@ """) shouldBe empty
      TestCodeSearcher[Suggestion]("""import "std/"@@ """) shouldBe empty
    }

    "cursor is right after the import statement" in {
      TestCodeSearcher[Suggestion]("""import@@ "std" """) shouldBe empty
      TestCodeSearcher[Suggestion]("""import@@ "std/" """) shouldBe empty
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
      TestCodeSearcher[Suggestion]("""import "@@" """) should contain theSameElementsAs expected
      TestCodeSearcher[Suggestion]("""import "s@@" """) should contain theSameElementsAs expected
      TestCodeSearcher[Suggestion]("""import "st@@" """) should contain theSameElementsAs expected
      TestCodeSearcher[Suggestion]("""import "std@@" """) should contain theSameElementsAs expected
    }

    "cursor is before the forward slash" in {
      TestCodeSearcher[Suggestion]("""import "@@std/" """) should contain theSameElementsAs expected
      TestCodeSearcher[Suggestion]("""import "s@@td/" """) should contain theSameElementsAs expected
      TestCodeSearcher[Suggestion]("""import "st@@d/" """) should contain theSameElementsAs expected
      TestCodeSearcher[Suggestion]("""import "std@@/" """) should contain theSameElementsAs expected
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
      TestCodeSearcher[Suggestion]("""import "std/@@" """) should contain theSameElementsAs expected
      TestCodeSearcher[Suggestion]("""import "std/fungible@@" """) should contain theSameElementsAs expected
      TestCodeSearcher[Suggestion]("""import "std/fungible_@@" """) should contain theSameElementsAs expected
      TestCodeSearcher[Suggestion]("""import "std/nft@@" """) should contain theSameElementsAs expected
      TestCodeSearcher[Suggestion]("""import "std/nft_@@" """) should contain theSameElementsAs expected
    }
  }
}
