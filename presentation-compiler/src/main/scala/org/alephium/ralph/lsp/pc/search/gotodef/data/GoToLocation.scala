package org.alephium.ralph.lsp.pc.search.gotodef.data

import org.alephium.ralph.Ast
import org.alephium.ralph.lsp.access.compiler.message.LineRange
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.SourceIndexExtension
import org.alephium.ralph.lsp.pc.sourcecode.SourceCodeState

import java.net.URI

object GoToLocation {

  /**
   * Converts the given data to a [[GoToLocation]].
   *
   * @param sourceCode The source file to navigate to.
   * @param asts       The positions within the source file to navigate to.
   * @return A Iterator over [[GoToLocation]]s representing the navigation destinations.
   */
  def apply(
      sourceCode: SourceCodeState.Parsed,
      asts: Iterator[Ast.Positioned]): Iterator[GoToLocation] =
    asts.flatMap(GoToLocation(_, sourceCode))

  /**
   * Converts the given data to a [[GoToLocation]].
   *
   * @param sourceCode The source file to navigate to.
   * @param ast        The position within the source file to navigate to.
   * @return A [[GoToLocation]] representing the navigation destination.
   */
  def apply(
      ast: Ast.Positioned,
      sourceCode: SourceCodeState.Parsed): Option[GoToLocation] =
    ast
      .sourceIndex
      .map {
        sourceIndex =>
          GoToLocation(
            uri = sourceCode.fileURI,
            lineRange = sourceIndex.toLineRange(sourceCode.code)
          )
      }

}

/**
 * Represents a location in source code that can be navigated to.
 *
 * @param uri       The URI of the source file.
 * @param lineRange The range of code within the source file.
 */
case class GoToLocation(
    uri: URI,
    lineRange: LineRange)
