// Copyright 2024 The Alephium Authors
// This file is part of the alephium project.
//
// The library is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// The library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with the library. If not, see http://www.gnu.org/licenses/.

package org.alephium.ralph.lsp.pc.search.completion

import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.client.TestClientLogger
import org.alephium.ralph.lsp.utils.log.ClientLogger
import org.alephium.ralph.lsp.pc.search.TestCodeProvider.suggest
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ImportCompleterSpec extends AnyWordSpec with Matchers {

  implicit val clientLogger: ClientLogger = TestClientLogger
  implicit val file: FileAccess           = FileAccess.disk
  implicit val compiler: CompilerAccess   = CompilerAccess.ralphc

  "return empty" when {
    "cursor is before the quotes" in {
      suggest("""import @@"std" """) shouldBe empty
      suggest("""import @@"std/" """) shouldBe empty
    }

    "cursor is after the quotes" in {
      suggest("""import "std"@@ """) shouldBe empty
      suggest("""import "std/"@@ """) shouldBe empty
    }

    "cursor is right after the import statement" in {
      suggest("""import@@ "std" """) shouldBe empty
      suggest("""import@@ "std/" """) shouldBe empty
    }
  }

  "suggest full imports" when {
    val expected =
      Array(
        "std/fungible_token_interface",
        "std/fungible_token_unimplemented",
        "std/nft_collection_interface",
        "std/nft_collection_with_royalty_interface",
        "std/nft_interface"
      ) map Suggestion.File

    "cursor is between the quotes" in {
      suggest("""import "@@" """) should contain theSameElementsAs expected
      suggest("""import "s@@" """) should contain theSameElementsAs expected
      suggest("""import "st@@" """) should contain theSameElementsAs expected
      suggest("""import "std@@" """) should contain theSameElementsAs expected
    }

    "cursor is before the forward slash" in {
      suggest("""import "@@std/" """) should contain theSameElementsAs expected
      suggest("""import "s@@td/" """) should contain theSameElementsAs expected
      suggest("""import "st@@d/" """) should contain theSameElementsAs expected
      suggest("""import "std@@/" """) should contain theSameElementsAs expected
    }
  }

  "suggest file names" when {
    val expected =
      Array(
        "fungible_token_interface",
        "fungible_token_unimplemented",
        "nft_collection_interface",
        "nft_collection_with_royalty_interface",
        "nft_interface"
      ) map Suggestion.File

    "cursor is after the forward slash" in {
      suggest("""import "std/@@" """) should contain theSameElementsAs expected
      suggest("""import "std/fungible@@" """) should contain theSameElementsAs expected
      suggest("""import "std/fungible_@@" """) should contain theSameElementsAs expected
      suggest("""import "std/nft@@" """) should contain theSameElementsAs expected
      suggest("""import "std/nft_@@" """) should contain theSameElementsAs expected
    }
  }

}
