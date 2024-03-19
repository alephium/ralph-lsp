package org.alephium.ralph.lsp.access.util

import scala.collection.mutable.ArrayBuffer
import org.alephium.ralph.lsp.access.compiler.message._

object StringUtil {

  /**
   * This method calculates the index of a character in the source code
   * given its line number and position within that line.
   * There are three types of line endings: \n, \r, \r\n as based on the
   * LSP specification.
   * See https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocuments
   *
   * The function stop as soon as the index is found.
   *
   * @param code      The source code
   * @param line      The line number of the character
   * @param character The position of the character within the line
   * @return The index of the character in the source code
   */
  def computeIndex(code: String,
                   line: Int,
                   character: Int): Int = {

      var lineNumber = 0
      var index = 0
      var prev: Character = null

      while (lineNumber < line && index < code.length) {
        val char = code(index)
        if (char == '\r') {
          if(prev == '\r') {
            lineNumber += 1
          }
          index += 1
        } else if (char == '\n') {
          lineNumber += 1
          index += 1
        } else {
          if(prev == '\r') {
            lineNumber += 1
          } else {
            index += 1
          }
        }
        prev = char
      }

      index + character
  }
}
