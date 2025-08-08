// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.cache

import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.client.TestClientLogger
import org.alephium.ralph.lsp.pc.workspace.TestWorkspace
import org.alephium.ralph.lsp.utils.log.ClientLogger
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.EitherValues._
import org.scalatest.OptionValues._

class SearchCacheSpec extends AnyWordSpec with Matchers {

  implicit val clientLogger: ClientLogger = TestClientLogger
  implicit val file: FileAccess           = FileAccess.disk
  implicit val compiler: CompilerAccess   = CompilerAccess.ralphc

  "Changes to `astSoft: LazyVal`" should {
    "not invalidate existing cache" in {
      // Generate a parsed workspace with one source file
      val code      = "Abstract Contract Test() {}"
      val workspace = TestWorkspace.genParsedOK(code).sample.value
      workspace.sourceCode should have size 1

      // The source code's `SoftAST` is not populated.
      val source = workspace.sourceCode.head
      source.astSoft.isEmpty shouldBe true

      // Create a search cache
      val searchCache = SearchCache(maxWorkspaces = 10)
      searchCache.size shouldBe 0

      def accessCache() = {
        // Search the cache for the `workspace`
        val result = searchCache.get(workspace)
        // The resulting cache's workspace is created workspace
        result.workspace shouldBe workspace
        // the number of cache entire are always 1
        searchCache.size shouldBe 1
      }

      // First access to the cache, this created an entry in the cache.
      accessCache()

      // Now populate the source's `SoftAST`.
      source.astSoft.fetch().value.toCode() shouldBe code
      source.astSoft.isDefined shouldBe true
      workspace.sourceCode.head.astSoft.isDefined shouldBe true

      // Second access to the cache, this will not create a new cache entry, the same workspace is returned.
      // To test this access create 2 entries, remove the functions `equalityFields` and `equals` from [[SourceCodeState.Parsed]]
      accessCache()

      TestWorkspace delete workspace
    }
  }

}
