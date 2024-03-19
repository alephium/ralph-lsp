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

    /** Convert [[SourceIndex]] that contains index information to [[LineRange]] that contains line and character information */
    def toLineRange(code: String): LineRange = {
      buildLineRange(code, sourceIndex.from, sourceIndex.to)
    }
  }

  /**
   * Constructs a line range from the provided source code, using the specified start and end indexes.
   * The while loop optimizes performance by stopping once the necessary indexes are found.
   *
   * Note: There's a special case to handle where a line ending can be either '\r' or '\r\n'.
   *
   * In the latter case, we need to avoid returning on '\r' alone, as '\n' completes the line ending.
   * Therefore, we always check the next character.
   *
   * If the 'to' index corresponds to the last character of the code, the loop terminates
   * after finding the end index without attempting to access code(i + 1), preventing an IndexOutOfBoundsException.
   *
   * @param code     The source code
   * @param from     The start index
   * @param to       The end index
   */

  def buildLineRange(code: String, from: Int, to: Int): LineRange = {
    if(from < 0 || to < 0 || to < from || from >= code.length || to >= code.length || code.length <= 1) {
      LineRange.zero
    } else {
      var line = 0
      var col = 0
      var index = 0
      var char: Character = null
      var next: Character = null

      var start: LinePosition = null
      var end: LinePosition = null

      @inline def newLine = {
        line += 1
        col = 0
        index+=1
        char = next
      }
      @inline def nextChar = {
        col += 1
        index+=1
        char = next
      }

      char = code(index)
      while (index < code.length && end == null) {
        if (index == from && start == null) {
          // Start index found
          start = LinePosition(line, col)
        }
        if (index == to && end == null) {
          // End index found, we stop the loop here
          end = LinePosition(line, col)
        } else {
          // We look one char forward to differentiate `\r\n` and `\r` line endings
          next = code(index + 1)
          if (char == '\r' && next == '\n') {
            nextChar
          } else if (char == '\r' && next != '\n') {
            newLine
          } else if (char == '\n') {
            newLine
          } else {
            nextChar
          }
        }
      }

      if(start == null || end == null){
        LineRange.zero
      } else {
        LineRange(start, end)
      }
    }
  }
}
