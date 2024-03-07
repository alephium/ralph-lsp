package org.alephium.ralph.lsp.access.compiler.message

import fastparse.IndexedParserInput
import org.alephium.ralph.{SourceIndex, SourcePosition}

object SourceIndexExtra {

  val zero: SourceIndex =
    org.alephium.ralph.SourceIndex.empty

  /**
   * Sending negative index to the client would be incorrect.
   * This set the index to be an empty range.
   *
   * This is a temporary solution until an AST is available for `ralph.json`.
   *
   * @see Issue <a href="https://github.com/alephium/ralph-lsp/issues/17">#17</a>.
   */
  def ensurePositive(index: Int, width: Int): SourceIndex =
    if (index < 0)
      zero
    else
      new SourceIndex(
        index = index,
        width = width
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
      val fastParseLineNumber = IndexedParserInput(code).prettyIndex(sourceIndex.from)
      val sourcePosition = SourcePosition.parse(fastParseLineNumber)

      val start = LinePosition(sourcePosition.rowIndex, sourcePosition.colIndex)
      val end = LinePosition(sourcePosition.rowIndex, sourcePosition.colIndex + sourceIndex.width)
      LineRange(start, end)
    }
  }
}
