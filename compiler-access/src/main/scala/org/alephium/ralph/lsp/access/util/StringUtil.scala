package org.alephium.ralph.lsp.access.util

import scala.collection.mutable.ArrayBuffer

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
  def computeIndex(code: String,
                   line: Int,
                   character: Int): Int = {

      var lineNumber = 0
      val lines = new ArrayBuffer[(String, Int)]()
      var sep = 0
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

  def previousComputeIndex(code: String,
    line: Int,
    character: Int): Int = {

      val lines = codeLines(code)
      var index = 0

      var i = 0
      while (i < line && i < lines.length) {
        val (l, sep) = lines(i)
        index += l.length + sep
        i += 1
      }

      index += character
      index
  }

  def codeLines(data: String): Array[(String, Int)] = {
    val lines = new ArrayBuffer[(String, Int)]()
    var line = ""
    var sep = 0
    var i = 0
    var prev: Character = null

    @inline def newLine = {
      lines += ((line, sep))
      line = ""
      sep = 0
    }

    while (i < data.length) {
      val char = data(i)
      if (char == '\r') {
        if(prev == '\r') {
          newLine
        }
        sep = sep + 1
      } else if (char == '\n') {
        sep = sep + 1
        newLine
      } else {
        if(prev == '\r') {
          newLine
        }
        line = line + char
      }
      prev = char
      i += 1
    }

    if(prev == '\r') newLine

    newLine

    lines.toArray
  }
}
