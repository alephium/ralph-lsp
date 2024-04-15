// Copyright 2024 The Alephium Authors
// This file is part of the alephium project.
//
// The library is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// The library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with the library. If not, see http://www.gnu.org/licenses/.

package org.alephium.ralph.lsp.access.util

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
  def computeIndex(
      code: String,
      line: Int,
      character: Int): Int = {

    var lineNumber      = 0
    var index           = 0
    var prev: Character = null

    while (lineNumber < line && index < code.length) {
      val char = code(index)
      if (char == '\r') {
        if (prev == '\r') {
          lineNumber += 1
        }
        index += 1
      } else if (char == '\n') {
        lineNumber += 1
        index += 1
      } else {
        if (prev == '\r') {
          lineNumber += 1
        } else {
          index += 1
        }
      }
      prev = char
    }

    index + character
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

  def buildLineRange(
      code: String,
      from: Int,
      to: Int): LineRange =
    if (from < 0 || to < 0 || to < from || from >= code.length || to >= code.length || code.length <= 1) {
      LineRange.zero
    } else {
      var line            = 0
      var col             = 0
      var index           = 0
      var char: Character = null
      var next: Character = null

      var start: LinePosition = null
      var end: LinePosition   = null

      @inline def newLine() = {
        line += 1
        col = 0
        index += 1
        char = next
      }
      @inline def nextChar() = {
        col += 1
        index += 1
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
            nextChar()
          } else if (char == '\r' && next != '\n') {
            newLine()
          } else if (char == '\n') {
            newLine()
          } else {
            nextChar()
          }
        }
      }

      if (start == null || end == null) {
        LineRange.zero
      } else {
        LineRange(start, end)
      }
    }

}
