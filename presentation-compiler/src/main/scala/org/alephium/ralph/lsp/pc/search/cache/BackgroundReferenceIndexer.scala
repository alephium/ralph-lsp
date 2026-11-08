// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.cache

import org.alephium.ralph.lsp.pc.workspace.WorkspaceState
import org.alephium.ralph.lsp.utils.log.{ClientLogger, StrictImplicitLogging}

import java.net.URI
import java.util.concurrent.{Callable, ExecutorService, FutureTask, LinkedBlockingQueue, ThreadFactory, ThreadPoolExecutor, TimeUnit}
import scala.collection.mutable
import scala.util.control.NonFatal

object BackgroundReferenceIndexer {

  def apply(): BackgroundReferenceIndexer =
    new BackgroundReferenceIndexer(
      new ThreadPoolExecutor(
        1,
        1,
        0L,
        TimeUnit.MILLISECONDS,
        new LinkedBlockingQueue[Runnable](),
        new ThreadFactory {
          override def newThread(task: Runnable): Thread = {
            val thread = new Thread(task, "ralph-reference-index")
            thread.setDaemon(true)
            thread
          }
        }
      )
    )

}

/** Event-driven owner of active snapshot caches. No timers, and at most one pending job per workspace. */
final class BackgroundReferenceIndexer private[cache] (executor: ExecutorService) extends AutoCloseable with StrictImplicitLogging {

  private final class Entry(
      val cache: WorkspaceSearchCache,
      val task: FutureTask[Unit])

  private val entries = mutable.Map.empty[URI, Entry]
  private var closed  = false

  def update(
      workspace: WorkspaceState.IsSourceAware,
      cache: => WorkspaceSearchCache
    )(implicit logger: ClientLogger): Unit = synchronized {
    if (!closed && !entries.get(workspace.workspaceURI).exists(_.cache.workspace == workspace)) {
      remove(workspace.workspaceURI)
      val snapshot = cache
      val task = new FutureTask[Unit](new Callable[Unit] {
        override def call(): Unit =
          try {
            snapshot.referenceIndex
            ()
          } catch {
            case _: java.util.concurrent.CancellationException => ()
            case NonFatal(error)                               => logger.error("Failed to build reference index", error)
          }
      })
      entries.put(workspace.workspaceURI, new Entry(snapshot, task))
      executor.execute(task)
    }
  }

  def remove(workspaceURI: URI): Unit = synchronized {
    entries.remove(workspaceURI).foreach {
      entry =>
        entry.task.cancel(true)
        executor match {
          case pool: ThreadPoolExecutor => pool.remove(entry.task); ()
          case _                        => ()
        }
    }
  }

  override def close(): Unit = synchronized {
    if (!closed) {
      closed = true
      entries.values.foreach(_.task.cancel(true))
      entries.clear()
      executor.shutdownNow()
      ()
    }
  }

}
