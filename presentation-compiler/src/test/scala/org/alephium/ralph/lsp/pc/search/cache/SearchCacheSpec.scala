// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.cache

import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.client.TestClientLogger
import org.alephium.ralph.lsp.pc.workspace.{TestWorkspace, Workspace, WorkspaceSearcher, WorkspaceState}
import org.alephium.ralph.lsp.pc.sourcecode.SourceCodeState
import org.alephium.ralph.lsp.pc.workspace.build.dependency.DependencyID
import scala.collection.immutable.ArraySeq
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

  "Incremental snapshots" should {
    "reuse unchanged file extraction and built-ins without forcing cold source trees" in {
      val workspace = TestWorkspace.genParsedOK("Abstract Contract First() {}", "Abstract Contract Second() {}").sample.value
      try {
        val cache  = SearchCache(1)
        val before = cache.get(workspace)
        workspace.sourceCode.foreach(_.astSoft.isEmpty shouldBe true)
        val firstTrees = before.trees
        val builtIns   = before.builtInTrees
        val changed = Workspace.parse(
          WorkspaceState.UnCompiled(workspace.build,
                                    ArraySeq(
                                      SourceCodeState.UnCompiled(workspace.sourceCode.head.fileURI, "Abstract Contract Changed() {}"),
                                      workspace.sourceCode(1)
                                    )
          )
        )
        val after = cache.get(changed)
        (after.sourceEntries(1) eq before.sourceEntries(1)) shouldBe true
        (after.sourceEntries.head eq before.sourceEntries.head) shouldBe false
        (after.builtInTrees eq builtIns) shouldBe true
        (after.dependencyTrees eq before.dependencyTrees) shouldBe true
        after.trees shouldBe WorkspaceSearcher.collectAllTreesSoft(changed)
        (after.sourceEntries(1).trees eq before.sourceEntries(1).trees) shouldBe true
        before.trees shouldBe firstTrees
        cache.get(changed) should be theSameInstanceAs after
      } finally {
        TestWorkspace.delete(workspace)
        ()
      }
    }

    "remove deleted and unparsed sources and recover after parser errors" in {
      val workspace = TestWorkspace.genParsedOK("Abstract Contract First() {}", "Abstract Contract Second() {}").sample.value
      try {
        val cache  = SearchCache(1)
        val before = cache.get(workspace)
        before.trees shouldBe WorkspaceSearcher.collectAllTreesSoft(workspace)
        val deleted     = workspace.copy(sourceCode = workspace.sourceCode.take(1))
        val afterDelete = cache.get(deleted)
        afterDelete.sourceEntries should have size 1
        afterDelete.trees.exists(_.parsed.fileURI == workspace.sourceCode(1).fileURI) shouldBe false
        val unparsed = WorkspaceState.UnCompiled(workspace.build, ArraySeq(SourceCodeState.OnDisk(workspace.sourceCode.head.fileURI)))
        cache.get(unparsed).sourceEntries shouldBe empty
        cache.get(unparsed).trees shouldBe WorkspaceSearcher.collectAllTreesSoft(unparsed)
        val errored = Workspace.parse(
          WorkspaceState.UnCompiled(workspace.build,
                                    ArraySeq(
                                      SourceCodeState.UnCompiled(workspace.sourceCode.head.fileURI, "Abstract Contract Broken(")
                                    )
          )
        )
        cache.get(errored).trees shouldBe WorkspaceSearcher.collectAllTreesSoft(errored)
        cache.get(workspace).trees shouldBe before.trees
      } finally {
        TestWorkspace.delete(workspace)
        ()
      }
    }

    "invalidate dependency scopes for import additions, removals and dependency changes" in {
      val workspace = TestWorkspace.genParsedOK("Abstract Contract First() {}").sample.value
      try {
        val cache      = SearchCache(1)
        val original   = cache.get(workspace)
        val oldTrees   = original.trees
        val std        = workspace.build.findDependency(DependencyID.Std).value
        val importPath = std.sourceCode.iterator.flatMap(_.parsed.importIdentifier).next().string.value
        val imported = Workspace.parse(
          WorkspaceState.UnCompiled(workspace.build,
                                    ArraySeq(
                                      SourceCodeState.UnCompiled(workspace.sourceCode.head.fileURI, s"import \"$importPath\"\nAbstract Contract First() {}")
                                    )
          )
        )
        cache.get(imported).trees shouldBe WorkspaceSearcher.collectAllTreesSoft(imported)
        cache.get(imported).trees.size should be > oldTrees.size
        val removed = WorkspaceState.UnCompiled(workspace.build, workspace.sourceCode)
        cache.get(removed).trees shouldBe oldTrees
        val noDependencies = workspace.copy(build = workspace.build.copy(dependencies = ArraySeq.empty))
        val fresh          = cache.get(noDependencies)
        fresh.builtInTrees shouldBe empty
        fresh.trees shouldBe WorkspaceSearcher.collectAllTreesSoft(noDependencies)
        fresh.trees.size should be < oldTrees.size
        original.trees shouldBe oldTrees
      } finally {
        TestWorkspace.delete(workspace)
        ()
      }
    }

    "keep roots independent and preserve correct results after retention eviction" in {
      val first  = TestWorkspace.genParsedOK("Abstract Contract First() {}").sample.value
      val second = TestWorkspace.genParsedOK("Abstract Contract Second() {}").sample.value
      try {
        val cache       = SearchCache(1)
        val firstCache  = cache.get(first)
        val secondCache = cache.get(second)
        cache.retainedWorkspaceCount shouldBe 1
        secondCache.trees shouldBe WorkspaceSearcher.collectAllTreesSoft(second)
        firstCache.trees shouldBe WorkspaceSearcher.collectAllTreesSoft(first)
        cache.get(first).trees shouldBe firstCache.trees
        secondCache.sourceEntries.head.source.fileURI should not be firstCache.sourceEntries.head.source.fileURI
        cache.release(first.workspaceURI)
        cache.retainedWorkspaceCount shouldBe 0
        cache.get(second).trees shouldBe secondCache.trees
        cache.clearRetained()
        cache.retainedWorkspaceCount shouldBe 0
      } finally {
        TestWorkspace.delete(first)
        TestWorkspace.delete(second)
        ()
      }
    }
  }

}
