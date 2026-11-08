// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST

import scala.collection.mutable

/** Reuses complete unchanged declarations while the surrounding grammar is parsed normally. */
object IncrementalSoftParser {

  private object ReuseKey

  private class Reuse(val declarations: Map[Int, SoftAST.BlockPartAST]) {

    var nodes      = 0
    var characters = 0

  }

  def parse(
      code: String,
      previousCode: String,
      previous: SoftAST.RootBlock): Parsed[SoftAST.RootBlock] =
    parseWithStats(code, previousCode, previous)._1

  /** Counts actual parser skips, rather than merely counting candidate nodes. */
  private[soft] def parseWithStats(
      code: String,
      previousCode: String,
      previous: SoftAST.RootBlock): (Parsed[SoftAST.RootBlock], Int, Int) = {
    val reuse = new Reuse(candidates(code, previousCode, previous))
    def parser[Unknown: P]: P[SoftAST.RootBlock] = {
      implicitly[P[Unknown]].misc.update(ReuseKey, reuse)
      SoftParser.parse
    }
    val parsed = fastparse.parse(code, parser(_))
    (parsed, reuse.nodes, reuse.characters)
  }

  /** A parse-run-local cache: no state is shared between files, requests or threads. */
  @noinline private[soft] def reuseOrElse[Unknown: P](parser: => P[SoftAST.BlockPartAST]): P[SoftAST.BlockPartAST] = {
    val ctx = implicitly[P[Unknown]]
    // Failure tracing must execute the grammar to collect its usual diagnostic messages.
    if (ctx.verboseFailures) parser
    else
      ctx.misc.get(ReuseKey) match {
        case Some(reuse: Reuse) =>
          reuse.declarations.get(ctx.index) match {
            case Some(declaration) =>
              reuse.nodes += 1
              reuse.characters += declaration.index.width
              // Cached Node wrappers contain mutable parent pointers. Copy syntax without sharing those wrappers.
              val copied = SoftAST.copyWithIndices(declaration)(identity)
              ctx.freshSuccess(copied, ctx.index + declaration.index.width)
            case None => parser
          }
        case _ => parser
      }
  }

  private def candidates(
      code: String,
      previousCode: String,
      previous: SoftAST.RootBlock): Map[Int, SoftAST.BlockPartAST] = {
    if (previous.index.index != 0 || previous.index.width != previousCode.length) return Map.empty
    var common = 0
    val limit  = math.min(code.length, previousCode.length)
    while (common < limit && code.charAt(common) == previousCode.charAt(common)) common += 1
    val result = mutable.Map.empty[Int, SoftAST.BlockPartAST]
    def collect(ast: SoftAST): Unit = {
      val declaration = ast match {
        case template: SoftAST.Template if template.block.isDefined => Some(template)
        case function: SoftAST.Function if function.block.isDefined => Some(function)
        case _                                                      => None
      }
      declaration match {
        case Some(value) if value.index.index >= 0 && value.index.index + value.index.width < common && complete(value) =>
          result.update(value.index.index, value)
        case _ => ast.children().foreach(collect)
      }
    }
    collect(previous)
    result.toMap
  }

  private def complete(ast: SoftAST): Boolean =
    !ast.isInstanceOf[SoftAST.ErrorAST] && ast.children().forall(complete)

}
