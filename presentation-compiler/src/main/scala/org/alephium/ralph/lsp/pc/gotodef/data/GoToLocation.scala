package org.alephium.ralph.lsp.pc.gotodef.data

import org.alephium.ralph.Ast
import org.alephium.ralph.lsp.access.compiler.message.CodeRange
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.SourceIndexExtension
import org.alephium.ralph.lsp.pc.sourcecode.SourceCodeState

import java.net.URI
import scala.collection.immutable.ArraySeq

object GoToLocation {

  /**
   * Converts the given data to a [[GoToLocation]].
   *
   * @param sourceCode The source file to navigate to.
   * @param ast        The positions within the source file to navigate to.
   * @return A list of [[GoToLocation]]s representing the navigation destinations.
   */
  def apply(sourceCode: SourceCodeState.Parsed,
            ast: ArraySeq[Ast.Positioned]): ArraySeq[GoToLocation] =
    ast.flatMap(GoToLocation(_, sourceCode))

  /**
   * Converts the given data to a [[GoToLocation]].
   *
   * @param sourceCode The source file to navigate to.
   * @param ast        The position within the source file to navigate to.
   * @return A [[GoToLocation]] representing the navigation destination.
   */
  def apply(ast: Ast.Positioned,
            sourceCode: SourceCodeState.Parsed): Option[GoToLocation] =
    ast
      .sourceIndex
      .map {
        sourceIndex =>
          GoToLocation(
            uri = sourceCode.fileURI,
            codeRange = sourceIndex.toCodeRange(sourceCode.code)
          )
      }
}

/**
 * Represents a location in source code that can be navigated to.
 *
 * @param uri       The URI of the source file.
 * @param codeRange The range of code within the source file.
 */
case class GoToLocation(uri: URI,
                        codeRange: CodeRange)
