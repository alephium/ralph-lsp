package org.alephium.ralph.lsp.access.compiler.message

import fastparse.IndexedParserInput
import org.alephium.ralph.{SourceIndex, SourcePosition}

import java.net.URI

object SourceIndexExtra {

  def zero(fileURI: URI): SourceIndex =
    SourceIndex(
      index = 0,
      width = 0,
      fileURI = Some(fileURI)
    )

  /**
   * Sending negative index to the client would be incorrect.
   * This set the index to be an empty range.
   *
   * This is a temporary solution until an AST is available for `ralph.json`.
   *
   * @see Issue <a href="https://github.com/alephium/ralph-lsp/issues/17">#17</a>.
   */
  def ensurePositive(index: Int,
                     width: Int,
                     fileURI: URI): SourceIndex =
    if (index < 0)
      zero(fileURI)
    else
      new SourceIndex(
        index = index,
        width = width,
        fileURI = Some(fileURI)
      )

  implicit class SourceIndexExtension(val sourceIndex: SourceIndex) extends AnyVal {
    def from: Int =
      sourceIndex.index

    def to: Int =
      sourceIndex.endIndex

    def width: Int =
      sourceIndex.width

    /** Checks if the given index is within this SourceIndex's from and to index */
    def contains(index: Int): Boolean =
      index >= from && index <= to

    /** Offset this SourceIndex */
    def +(right: Int): SourceIndex =
      sourceIndex.copy(index = from + right)

    /** Convert [[SourceIndex]] that contains index information to [[LineRange]] that contains line and character information */
    def toLineRange(code: String): LineRange = {
      val parserInput = IndexedParserInput(code)

      // Build the 'from' line position information
      val fromLineNumber = parserInput.prettyIndex(sourceIndex.from)
      val fromSourcePosition = SourcePosition.parse(fromLineNumber)
      val from = LinePosition(fromSourcePosition.rowIndex, fromSourcePosition.colIndex)

      // Build the 'to' line position information
      val toLineNumber = parserInput.prettyIndex(sourceIndex.to)
      val toSourcePosition = SourcePosition.parse(toLineNumber)
      val to = LinePosition(toSourcePosition.rowIndex, toSourcePosition.colIndex)

      LineRange(from, to)
    }
  }
}
