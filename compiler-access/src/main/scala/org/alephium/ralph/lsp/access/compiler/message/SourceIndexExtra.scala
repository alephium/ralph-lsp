package org.alephium.ralph.lsp.access.compiler.message

import fastparse.IndexedParserInput
import org.alephium.ralph.{SourceIndex, SourcePosition}
import org.alephium.ralph.lsp.access.util.StringUtil._

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

    def linePosition(lineNumberLookup: Seq[Int], index:Int): LinePosition = {
      val line = lineNumberLookup.indexWhere(_ > index) match {
        case -1 => lineNumberLookup.length - 1
        case n => math.max(0, n - 1)
      }
      val col = index - lineNumberLookup(line)
      LinePosition(line, col)
    }
    /** Convert [[SourceIndex]] that contains index information to [[LineRange]] that contains line and character information */
    def toLineRange(code: String): LineRange = {
      /*
       * Our current fastparse version is having issue with return lines on Windows.
       * The following workaround is inspired by fastparse ParserInput.prettyIndex
       */

      val separatorLength = lineSeparatorLength(code)
      val lineNumberLookup =
        codeLines(code)
         .foldLeft(Seq(0))((acc, line) => acc :+ (acc.last + line.size + separatorLength))

      val start = linePosition(lineNumberLookup, sourceIndex.from)
      val end = linePosition(lineNumberLookup, sourceIndex.to)

      LineRange(start, end)
    }
  }
}
