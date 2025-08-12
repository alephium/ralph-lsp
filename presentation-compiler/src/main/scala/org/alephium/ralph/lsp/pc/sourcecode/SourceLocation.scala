// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.sourcecode

import org.alephium.ralph.{Ast, SourceIndex}
import org.alephium.ralph.lsp.access.compiler.ast.Tree
import org.alephium.ralph.lsp.access.compiler.message.{LinePosition, LineRange}
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.SourceIndexExtension
import org.alephium.ralph.lsp.access.compiler.message.error.FastParseError
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST
import org.alephium.ralph.lsp.utils.log.{ClientLogger, StrictImplicitLogging}

import scala.collection.immutable.ArraySeq

/** Represents a position within a source-file in parsed state. */
sealed trait SourceLocation {

  def parsed: SourceCodeState.IsParsedAndCompiled

}

object SourceLocation extends StrictImplicitLogging {

  /**
   * Ensures that only unique definitions for a file and range are kept.
   *
   * @note [[GoTo.index]] must be defined.
   *       The [[GoTo.index]] is optional because [[Ast.Positioned.sourceIndex]] is optional.
   *       However, [[SoftAST.index]] is always defined.
   *       Ensure that [[SourceIndex]] from [[Ast.Positioned]] is defined in the input.
   * @return Unique results based on file URI and range.
   */
  def distinctByLocation[A <: GoTo](goTo: ArraySeq[A])(implicit logger: ClientLogger): ArraySeq[A] =
    goTo.distinctBy {
      goToDef =>
        val index =
          goToDef.index.map {
            sourceIndex =>
              (sourceIndex.from, sourceIndex.to)
          }

        // Report to debug cases where index is `None`
        if (index.isEmpty)
          logger.error(s"`${goToDef.getClass.getName}` contains `None` source-index. FileURI: `${goToDef.parsed.fileURI}`")

        (index, goToDef.parsed.fileURI)
    }

  /**
   * Result types for GoTo location search results.
   */
  sealed trait GoTo extends SourceLocation {

    def index: Option[SourceIndex]

    def toLineRange(): Option[LineRange]

  }

  /**
   * Result types for GoTo definition location search results.
   */
  sealed trait GoToDef       extends GoTo
  sealed trait GoToDefStrict extends GoToDef
  sealed trait GoToDefSoft   extends GoToDef

  /**
   * Result types for renaming location search results.
   */
  sealed trait GoToRename       extends GoTo
  sealed trait GoToRenameStrict extends GoToRename
  sealed trait GoToRenameSoft   extends GoToRename

  /**
   * Result types for GoTo references location search results.
   */
  sealed trait GoToRef       extends GoTo
  sealed trait GoToRefStrict extends GoToRef
  sealed trait GoToRefSoft   extends GoToRef

  /**
   * Represents a source file ([[SourceCodeState.Parsed]]) without
   * a target position. For eg: Used to provide jump definition for imported files.
   *
   * @param parsed The source file containing the positioned node.
   */
  case class File(
      parsed: SourceCodeState.IsParsedAndCompiled)
    extends GoToDefStrict
       with GoToDefSoft {

    def lineRange(): LineRange =
      LineRange.zero

    override def toLineRange(): Option[LineRange] =
      Some(lineRange())

    override def index: Option[SourceIndex] =
      Some(SourceIndex.empty)

  }

  /**
   * Represents a position with an import statement.
   *
   * @param name   The name of the file or folder.
   * @param parsed The parsed source source-code containing the import statement.
   */
  case class ImportName(
      name: Tree.Name,
      parsed: SourceCodeState.Parsed)
    extends GoToRefStrict
       with GoToRenameStrict {

    override def index: Option[SourceIndex] =
      Some(name.index)

    def lineRange(): LineRange =
      name.index.toLineRange(parsed.code)

    override def toLineRange(): Option[LineRange] =
      Some(lineRange())

  }

  /**
   * Represents a single positioned AST ([[org.alephium.ralph.Ast.Positioned]])
   * within a source tree ([[SourceLocation.CodeStrict]]),
   *
   * @param ast    The positioned node within the parsed source file.
   * @param source The source tree containing the positioned node.
   */
  case class NodeStrict[+A <: Ast.Positioned](
      ast: A,
      source: CodeStrict)
    extends GoToDefStrict
       with GoToRefStrict
       with GoToRenameStrict {

    override def index: Option[SourceIndex] =
      ast.sourceIndex

    def toLineRange(): Option[LineRange] =
      index.map(_.toLineRange(source.parsed.code))

    override def parsed: SourceCodeState.Parsed =
      source.parsed

  }

  case class NodeSoft[+A <: SoftAST](
      ast: A,
      source: CodeSoft)
    extends GoToDefSoft
       with GoToRefSoft
       with GoToRenameSoft {

    override def index: Option[SourceIndex] =
      Some(ast.index)

    def toLineRange(): Option[LineRange] =
      Some(ast.index.toLineRange(source.parsed.code))

    override def parsed: SourceCodeState.IsParsedAndCompiled =
      source.parsed

    /**
     * Converts this [[SoftAST]] result to Strict-AST's result.
     *
     * @return a node representing Strict-AST.
     */
    def toNodeStrict()(implicit logger: ClientLogger): Option[NodeStrict[Ast.Positioned]] =
      source.parsed match {
        case parsed: SourceCodeState.Parsed =>
          parsed
            .astStrict
            .statements
            .find(_.index containsSoft ast.index)
            .flatMap {
              case tree: Tree.Import =>
                // NodeSoft and NodeStrict do not point to a `File`. Files (import go-to-def) are stored in the `File` type.
                logger.error(s"Expected: ${Tree.Source.getClass.getName}. Actual: ${tree.getClass.getName}")
                None

              case tree: Tree.Source =>
                tree
                  .rootNode
                  .findLast(_.sourceIndex.exists(_ isEqualToSoft ast.index))
                  .map {
                    node =>
                      NodeStrict(
                        ast = node.data,
                        source = CodeStrict(
                          tree = tree,
                          parsed = parsed
                        )
                      )
                  }
            }

        case _ =>
          None
      }

  }

  case class GoToTypeDef(
      ast: Ast.TypeId,
      source: CodeStrict)
    extends GoTo {

    override def index: Option[SourceIndex] =
      ast.sourceIndex

    def toLineRange(): Option[LineRange] =
      index.map(_.toLineRange(source.parsed.code))

    override def parsed: SourceCodeState.Parsed =
      source.parsed

  }

  /**
   * Represents information for inlay-hints.
   *
   * @param position  Position where the inlay-hint should be displayed.
   * @param parsed    Source code where the inlay-hint is to be shown.
   * @param hint The string content of the hint to be displayed inline (e.g. the inferred type name).
   * @param typeDef   The type definition associated with the hint.
   */
  case class InlayHint(
      position: SourceIndex,
      parsed: SourceCodeState.Parsed,
      hint: String,
      typeDef: GoToTypeDef)
    extends GoTo {

    override def index: Option[SourceIndex] =
      Some(position)

    def toLineRange(): Option[LineRange] =
      index.map(_.toLineRange(parsed.code))

    /** The position of the hint to be displayed inline */
    def inlayHintPosition(): Option[LinePosition] =
      toLineRange().map(_.from)

  }

  /**
   * Represents information for Hover.
   *
   * @param content    The ast containing the content to be displayed on hover.
   * @param code       The source file containing the source tree.
   * @param initialAST The AST that was used to generate the hover content.
   */
  case class Hover(
      content: SoftAST,
      code: CodeSoft,
      initialAST: SoftAST)
    extends GoTo {

    override def index: Option[SourceIndex] =
      Some(initialAST.index)

    override def toLineRange(): Option[LineRange] =
      None

    override def parsed: SourceCodeState.IsParsedAndCompiled =
      code.parsed

  }

  sealed trait Code extends SourceLocation {

    def parsed: SourceCodeState.IsParsedAndCompiled

  }

  /**
   * Represents a single source tree ([[Tree.Source]]) within a source file ([[SourceCodeState.Parsed]]),
   * which can contain multiple source trees such as contracts, scripts etc.
   *
   * @param tree   The source tree within the parsed source file.
   * @param parsed The source file containing the source tree.
   */
  case class CodeStrict(
      tree: Tree.Source,
      parsed: SourceCodeState.Parsed)
    extends Code
       with StrictImplicitLogging {

    /** Converts this [[CodeStrict]] instance to [[CodeSoft]] by finding the same tree at that index. */
    def toCodeSoft()(implicit logger: ClientLogger): Option[CodeSoft] =
      toSoftAST() match {
        case Right(part) =>
          part map {
            part =>
              CodeSoft(
                part = part,
                parsed = parsed
              )
          }

        case Left(error) =>
          logger.error(error.error.toFormatter(parsed.code).format())
          None
      }

    /** Converts this [[Tree.Source]] instance to [[SoftAST.BlockPartAST]] by finding the same tree at that index. */
    private def toSoftAST(): Either[FastParseError, Option[SoftAST.BlockPartAST]] =
      parsed
        .astSoft
        .fetch()
        .map(_.parts.find(_.index containsSoft tree.index))

  }

  case class CodeSoft(
      part: SoftAST.BlockPartAST,
      parsed: SourceCodeState.IsParsedAndCompiled)
    extends Code

}
