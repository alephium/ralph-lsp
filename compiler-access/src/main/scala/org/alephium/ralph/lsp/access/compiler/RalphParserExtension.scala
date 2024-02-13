package org.alephium.ralph.lsp.access.compiler

import fastparse._
import org.alephium.ralph.StatefulParser.{rawContract, rawInterface, rawTxScript, whitespace}
import org.alephium.ralph.lsp.access.compiler.ast.Tree
import org.alephium.ralph.lsp.access.compiler.message.SourceIndex

/** Functions that extend ralphc's default parser */
private object RalphParserExtension {

  /**
   * An extension to Ralphc's parse function [[org.alephium.ralph.StatefulParser.multiContract]]
   * that add support for import syntax.
   */
  def multiContract[Unknown: P]: P[Tree.Root] =
    P(Start ~ Index ~ statement.rep(1) ~ Index ~ End) map {
      case (fromIndex, statements, toIndex) =>
        val index =
          SourceIndex(
            index = fromIndex,
            width = toIndex - fromIndex
          )

        Tree.Root(
          statements = statements,
          index = index
        )
    }

  /** A statement can be an import or ralphc's contract */
  private def statement[Unknown: P]: P[Tree.Statement] =
    P(importStatement | sourceStatement)

  /** Parse import syntax */
  private def importStatement[Unknown: P]: P[Tree.Import] =
    P(Index ~ "import" ~ stringLiteral ~ Index) map {
      case (fromIndex, importPackage, toIndex) =>
        val index =
          SourceIndex(
            index = fromIndex,
            width = toIndex - fromIndex
          )

        Tree.Import(
          index = index,
          pkg = importPackage
        )
    }

  /**
   *
   * Parse a ralphc contract.
   *
   * This function is a clone of [[org.alephium.ralph.StatefulParser.multiContract]]
   * but without the requirement that it be the start of the file, so imports are allowed.
   * */
  private def sourceStatement[Unknown: P]: P[Tree.Source] =
    P(Index ~ (rawTxScript | rawContract | rawInterface) ~ Index) map {
      case (fromIndex, code, toIndex) =>
        val index =
          SourceIndex(
            index = fromIndex,
            width = toIndex - fromIndex
          )

        Tree.Source(
          ast = code,
          index = index
        )
    }

  /**
   * A string literal. For example in the following code
   * `"package_name/file"` is a string literal.
   *
   * {{{
   *   import "package_name/file"
   * }}}
   * */
  private def stringLiteral[Unknown: P]: P[Tree.StringLiteral] =
    P(Index ~ "\"" ~~ CharsWhile(_ != '"').! ~~ "\"" ~ Index) map { // TODO: See if negative look ahead with AnyChar.rep would work instead of `CharsWhile`
      case (fromIndex, string, toIndex) =>
        val index =
          SourceIndex(
            index = fromIndex,
            width = toIndex - fromIndex
          )

        Tree.StringLiteral(
          value = string,
          index = index
        )
    }
}
