package org.alephium.ralph.lsp.pc.util

object StringUtil {

  /**
   * This method calculates the index of a character in the source code
   * given its line number and position within that line.
   *
   * @param lines     An array containing each line of the source code
   * @param line      The line number of the character
   * @param character The position of the character within the line
   * @return The index of the character in the source code
   */
  def computeIndex(lines: Array[String],
                   line: Int,
                   character: Int): Int = {
    var index = 0

    var i = 0
    while (i < line && i < lines.length) {
      index += lines(i).length + System.lineSeparator().length
      i += 1
    }

    index += character
    index
  }

}
