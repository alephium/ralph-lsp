// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.cache

import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.client.TestClientLogger
import org.alephium.ralph.lsp.pc.workspace.TestWorkspace
import org.alephium.ralph.lsp.utils.log.ClientLogger
import org.alephium.ralph.lsp.utils.LazyVal
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.OptionValues._

import java.util
import java.util.concurrent.{AbstractExecutorService, Callable, CountDownLatch, Executors, FutureTask, TimeUnit}
import scala.collection.mutable.ArrayBuffer

class BackgroundReferenceIndexerSpec extends AnyWordSpec with Matchers {

  implicit val clientLogger: ClientLogger = TestClientLogger
  implicit val file: FileAccess           = FileAccess.disk
  implicit val compiler: CompilerAccess   = CompilerAccess.ralphc

  private class ManualExecutor extends AbstractExecutorService {

    val tasks                                       = ArrayBuffer.empty[Runnable]
    private var closed                              = false
    override def execute(task: Runnable): Unit      = { tasks += task; () }
    override def shutdown(): Unit                   = closed = true
    override def shutdownNow(): util.List[Runnable] = { shutdown(); new util.ArrayList[Runnable]() }
    override def isShutdown: Boolean                = closed
    override def isTerminated: Boolean              = closed

    override def awaitTermination(
        timeout: Long,
        unit: TimeUnit): Boolean = closed

  }

  "Background reference indexer" should {
    "schedule only changed snapshots and cancel superseded queued work" in {
      val workspace = TestWorkspace.genParsedOK("Abstract Contract One() {}", "Abstract Contract Two() {}").sample.value
      val executor  = new ManualExecutor
      val indexer   = new BackgroundReferenceIndexer(executor)
      try {
        val first = new WorkspaceSearchCache(workspace)
        indexer.update(workspace, first)
        workspace.sourceCode.head.astSoft.isEmpty shouldBe true
        indexer.update(workspace.copy(), fail("unchanged snapshot must not allocate another cache"))
        executor.tasks should have size 1
        val edited = workspace.copy(sourceCode = workspace.sourceCode.take(1))
        val second = new WorkspaceSearchCache(edited)
        indexer.update(edited, second)
        executor.tasks should have size 2
        executor.tasks.head.asInstanceOf[FutureTask[Unit]].isCancelled shouldBe true
        executor.tasks.foreach(_.run())
        second.referenceIndex.candidates(Seq("One")) should have size 1
        second.referenceIndex.candidates(Seq("Two")) shouldBe empty
      } finally {
        indexer.close()
        TestWorkspace.delete(workspace)
        ()
      }
    }

    "interrupt an obsolete running build and allow the newest snapshot to finish" in {
      val workspace   = TestWorkspace.genParsedOK("Abstract Contract One() {}", "Abstract Contract Two() {}").sample.value
      val entered     = new CountDownLatch(1)
      val interrupted = new CountDownLatch(1)
      val release     = new CountDownLatch(1)
      val original    = workspace.sourceCode.head
      val blocked = original.copy(astSoft = LazyVal {
        entered.countDown()
        try {
          release.await()
          original.astSoft.fetch()
        } finally interrupted.countDown()
      })
      val old      = workspace.copy(sourceCode = workspace.sourceCode.updated(0, blocked))
      val executor = Executors.newSingleThreadExecutor()
      val indexer  = new BackgroundReferenceIndexer(executor)
      try {
        indexer.update(old, new WorkspaceSearchCache(old))
        entered.await(30, TimeUnit.SECONDS) shouldBe true
        val edited  = workspace.copy(sourceCode = workspace.sourceCode.drop(1))
        val current = new WorkspaceSearchCache(edited)
        indexer.update(edited, current)
        interrupted.await(30, TimeUnit.SECONDS) shouldBe true
        executor.submit(new Callable[Unit] { override def call(): Unit = () }).get(30, TimeUnit.SECONDS)
        current.referenceIndex.candidates(Seq("One")) shouldBe empty
        current.referenceIndex.candidates(Seq("Two")) should have size 1
      } finally {
        release.countDown()
        indexer.close()
        executor.awaitTermination(30, TimeUnit.SECONDS) shouldBe true
        TestWorkspace.delete(workspace)
        ()
      }
    }

    "cancel removal and shutdown without accepting further updates" in {
      val workspace = TestWorkspace.genParsedOK("Abstract Contract One() {}").sample.value
      val executor  = new ManualExecutor
      val indexer   = new BackgroundReferenceIndexer(executor)
      try {
        indexer.update(workspace, new WorkspaceSearchCache(workspace))
        indexer.remove(workspace.workspaceURI)
        executor.tasks.head.asInstanceOf[FutureTask[Unit]].isCancelled shouldBe true
        indexer.update(workspace, new WorkspaceSearchCache(workspace))
        indexer.close()
        indexer.close()
        executor.isShutdown shouldBe true
        executor.tasks.last.asInstanceOf[FutureTask[Unit]].isCancelled shouldBe true
        indexer.update(workspace, fail("closed owner must not allocate a cache"))
        executor.tasks should have size 2
      } finally { TestWorkspace.delete(workspace); () }
    }
  }

}
