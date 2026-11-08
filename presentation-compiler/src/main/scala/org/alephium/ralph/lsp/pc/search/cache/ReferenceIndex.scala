// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.cache

import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST
import org.alephium.ralph.lsp.pc.sourcecode.SourceLocation
import org.alephium.ralph.lsp.pc.workspace.{WorkspaceSearcher, WorkspaceState}
import org.alephium.ralph.lsp.utils.Node
import org.alephium.ralph.lsp.utils.log.{ClientLogger, StrictImplicitLogging}

import java.util.concurrent.CancellationException
import scala.collection.immutable.ArraySeq
import scala.collection.mutable

/** Immutable identifier postings for exactly one workspace snapshot, including dependencies. */
final class ReferenceIndex private (postings: Map[String, ArraySeq[(Int, SourceLocation.NodeSoft[SoftAST.Identifier])]]) {

  /** Preserve source traversal order without visiting unrelated identifiers. */
  def candidates(names: Iterable[String]): ArraySeq[SourceLocation.NodeSoft[SoftAST.Identifier]] =
    names
      .iterator
      .toSet
      .iterator
      .flatMap(
        (name: String) => postings.getOrElse(name, ArraySeq.empty)
      )
      .to(ArraySeq)
      .sortBy(_._1)
      .map(_._2)

}

object ReferenceIndex extends StrictImplicitLogging {

  def build(
      workspace: WorkspaceState.IsSourceAware,
      isCancelled: () => Boolean = () => Thread.currentThread().isInterrupted
    )(implicit logger: ClientLogger): ReferenceIndex = {
    val postings = mutable.Map.empty[String, mutable.ArrayBuffer[(Int, SourceLocation.NodeSoft[SoftAST.Identifier])]]
    var ordinal  = 0
    def checkCancelled(): Unit =
      if (isCancelled()) throw new CancellationException("Reference indexing cancelled")

    WorkspaceSearcher.collectAllIsParsed(workspace).foreach {
      parsed =>
        checkCancelled()
        parsed.astSoft.fetch() match {
          case Right(block) =>
            block.parts.foreach {
              part =>
                checkCancelled()
                part.toNode.walk.foreach {
                  node =>
                    checkCancelled()
                    node match {
                      case Node(identifier: SoftAST.Identifier, _) =>
                        val location = SourceLocation.NodeSoft(identifier, SourceLocation.CodeSoft(part, parsed))
                        postings.getOrElseUpdate(identifier.code.text, mutable.ArrayBuffer.empty) += ((ordinal, location))
                        ordinal += 1
                      case _ => ()
                    }
                }
            }
          case Left(error) =>
            logger.error(s"SoftParser Error: Failed to index source code. File: ${parsed.fileURI}. Error Message: ${error.message}")
        }
    }
    checkCancelled()
    new ReferenceIndex(
      postings
        .iterator
        .map {
          case (name, entries) => name -> entries.to(ArraySeq)
        }
        .toMap
    )
  }

}
