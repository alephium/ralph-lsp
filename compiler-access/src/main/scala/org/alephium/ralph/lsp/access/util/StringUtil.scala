package org.alephium.ralph.lsp.access.util

object StringUtil {

  /**
   * This method calculates the index of a character in the source code
   * given its line number and position within that line.
   *
   * @param code      The source code
   * @param line      The line number of the character
   * @param character The position of the character within the line
   * @return The index of the character in the source code
   */
  def computeIndex(code: String, line: Int, character: Int): Int = {

    val separatorLength = lineSeparatorLength(code)
    val lines = codeLines(code)
    var index = 0

    var i = 0
    while (i < line && i < lines.length) {
      index += lines(i).length + separatorLength
      i += 1
    }

    index += character
    index
  }

  def codeLines(code: String): Array[String] =
    code.split("\r\n|\r|\n")

  def lineSeparatorLength(code: String): Int =
    if (code.contains("\r\n")) 2 else 1
}
