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
      /*
       * Our current fastparse version is having issue with return lines on Windows.
       * The following workaround is inspired by fastparse ParserInput.prettyIndex
       */
      val lineNumberLookup =
        code
         .split(System.lineSeparator())
         .foldLeft(Seq(0))((acc, line) => acc :+ (acc.last +line.size + System.lineSeparator().length))

      val line = lineNumberLookup.indexWhere(_ > sourceIndex.from) match {
        case -1 => lineNumberLookup.length - 1
        case n => math.max(0, n - 1)
      }
      val col = sourceIndex.from - lineNumberLookup(line)

      val start = LinePosition(line, col)
      val end = LinePosition(line, col + sourceIndex.width)
      LineRange(start, end)
    }
  }
}
