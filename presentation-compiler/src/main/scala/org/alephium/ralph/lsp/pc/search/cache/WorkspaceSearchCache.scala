// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.cache

import org.alephium.ralph.lsp.pc.sourcecode.{SourceCodeSearcher, SourceCodeState, SourceLocation}
import org.alephium.ralph.lsp.pc.workspace.{WorkspaceSearcher, WorkspaceState}
import org.alephium.ralph.lsp.pc.workspace.build.BuildState
import org.alephium.ralph.lsp.pc.workspace.build.dependency.DependencyID
import org.alephium.ralph.lsp.utils.log.ClientLogger

import scala.collection.immutable.ArraySeq

object WorkspaceSearchCache {

  def apply(workspaceState: WorkspaceState.IsSourceAware)(implicit logger: ClientLogger): WorkspaceSearchCache =
    new WorkspaceSearchCache(workspaceState)

  /** Extraction is lazy per file, and survives changes to other files. */
  private[cache] final class SourceEntry(val source: SourceCodeState.IsParsed)(implicit logger: ClientLogger) {

    lazy val trees: ArraySeq[SourceLocation.CodeSoft] =
      SourceCodeSearcher.collectSourceTreesSoft(source).to(ArraySeq)

    lazy val imports: Set[String] =
      SourceCodeSearcher.collectImportStatementsSoft(ArraySeq(source)).flatMap(_.string).map(_.toCode()).toSet

  }

  /** A build's dependency scope is independent of edits to local declarations. */
  private[cache] final class Dependencies(build: BuildState.Compiled)(implicit logger: ClientLogger) {

    // Retain only the most recently requested import set, not an unbounded edit history.
    private var imported: Option[(Set[String], ArraySeq[SourceLocation.CodeSoft])] = None

    def trees(imports: Set[String]): ArraySeq[SourceLocation.CodeSoft] =
      synchronized {
        imported match {
          case Some((previous, trees)) if previous == imports => trees
          case _ =>
            val trees = WorkspaceSearcher.collectImportedTreesSoft(build, imports)
            imported = Some((imports, trees))
            trees
        }
      }

    lazy val builtInTrees: ArraySeq[SourceLocation.CodeSoft] =
      WorkspaceSearcher.collectAllDependencyTreesSoft(DependencyID.BuiltIn, build) match {
        case Some((builtIn, trees)) => trees.filter(!_.parsed.isPrimitive(builtIn))
        case None                   => ArraySeq.empty
      }

  }

}

/**
 * An immutable workspace snapshot with reusable per-file extraction and dependency scopes.
 * Constructing a successor does not force parsing, and never retains its predecessor.
 */
final class WorkspaceSearchCache(
    val workspace: WorkspaceState.IsSourceAware,
    previous: Option[WorkspaceSearchCache] = None
  )(implicit logger: ClientLogger) {

  import WorkspaceSearchCache._

  private[cache] val sourceEntries: ArraySeq[SourceEntry] = {
    val existing = previous
      .iterator
      .flatMap(_.sourceEntries)
      .map(
        entry => entry.source.fileURI -> entry
      )
      .toMap
    SourceCodeSearcher.collectIsParsed(workspace.sourceCode).map {
      source =>
        existing
          .get(source.fileURI)
          .filter(
            entry => (entry.source eq source) || entry.source == source
          )
          .getOrElse(new SourceEntry(source))
    }
  }

  private val dependencies: Dependencies =
    previous.filter(_.workspace.build == workspace.build).map(_.dependencies).getOrElse(new Dependencies(workspace.build))

  /** Local source order and imported/inherited/primitive scope match the uncached search. */
  lazy val trees: ArraySeq[SourceLocation.CodeSoft] =
    sourceEntries.flatMap(_.trees) ++ dependencyTrees

  private[cache] lazy val dependencyTrees: ArraySeq[SourceLocation.CodeSoft] =
    dependencies.trees(sourceEntries.iterator.flatMap(_.imports).toSet)

  lazy val builtInTrees: ArraySeq[SourceLocation.CodeSoft] =
    dependencies.builtInTrees

}
